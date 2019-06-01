{-# LANGUAGE OverloadedStrings #-}
module Hedgehog.WebDriver.Commands
( MonadWebTest
-- ** Awaiting for elements
, awaitElem, awaitElemWithin, awaitElemWithin'
, awaitText
, awaitDisplayed, awaitNotDisplayed
, awaitEnabled, awaitDisabled
, awaitMissing

-- ** Session operations
, cleanupSession

-- ** Lower level primitives
, MonadWebDriver
, HasElement
, findAll
)
where

import Control.Monad          (filterM, void)
import Control.Monad.Catch    (MonadCatch, catch, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty     (NonEmpty, nonEmpty)

import Data.Bifunctor                       (bimap)
import Data.Bool                            (bool)
import Data.Either                          (isRight)
import Data.Functor                         ((<&>))
import Data.Maybe                           (fromMaybe, listToMaybe)
import Data.Text                            (Text)
import Hedgehog                             (MonadTest, diff, evalEither, evalM)
import Hedgehog.Internal.Show               (showPretty)
import Hedgehog.Internal.Source             (HasCallStack (..), withFrozenCallStack)
import Hedgehog.WebDriver.Internal.Commands
import Hedgehog.WebDriver.Internal.Retry    (retrying, retryingMap)
import Hedgehog.WebDriver.WebContext        (Millis (..), MonadWebDriver, MonadWebTest, WebContext (..), WebContextState (..))

import qualified Data.List.NonEmpty           as Nel
import qualified Data.Text                    as Text
import           Test.WebDriver               (Element, FailedCommand (..), FailedCommandType (..), Selector (..))
import qualified Test.WebDriver               as Web
import           Test.WebDriver.Class         (WebDriver)
import qualified Test.WebDriver.Commands      as Web
import qualified Test.WebDriver.Commands.Wait as Wait

-- | Performs a session cleanup by deleting everything from WebStorage and
-- deleting all the visible cookies.
cleanupSession :: (WebDriver m, WebContextState m) => m ()
cleanupSession = do
  Web.deleteAllKeys Web.LocalStorage
  Web.deleteAllKeys Web.SessionStorage
  Web.deleteVisibleCookies

-- | The same as 'Test.WebDriver.findElem', but awaits for the element
-- to appear on the web page, giving time to the page
-- and to the scripts to run.
awaitElem :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitElem a = withFrozenCallStack $ awaitElemWithin' Nothing a (pure . const True)

awaitElemWithin :: (HasElement a, MonadWebTest m, HasCallStack) => Maybe Element -> a -> m Element
awaitElemWithin root a = withFrozenCallStack $ awaitElemWithin' root a (pure . const True)

awaitElemWithin' :: (HasElement a, MonadWebTest m, HasCallStack)
  => Maybe Element
  -> a
  -> (Element -> m Bool)
  -> m Element
awaitElemWithin' = withFrozenCallStack $ awaitElementWithErr Nothing

-- | Awaits for an element that is visible on the page
awaitDisplayed :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitDisplayed a = withFrozenCallStack $
  awaitElementWithErr (Just "displayed") Nothing a Web.isDisplayed

awaitNotDisplayed :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitNotDisplayed a = withFrozenCallStack $
  awaitElementWithErr (Just "not displayed") Nothing a (fmap not . Web.isDisplayed)

-- | Awaits for the element to be enabled
awaitEnabled :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitEnabled a = withFrozenCallStack $
  awaitElementWithErr (Just "enabled") Nothing a Web.isEnabled

-- | Awaits for the element to be disabled
awaitDisabled :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitDisabled a = withFrozenCallStack $
  awaitElementWithErr (Just "disabled") Nothing a (fmap not . Web.isEnabled)

awaitMissing :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m ()
awaitMissing a = withFrozenCallStack $ evalM $
  retrying (pure . const True) (touchElem `catch` handler)
    <&> bimap (const errorMsg) id
    >>= evalEither
  where
    errorMsg = "Element is expected to disappear from DOM, but it is still there"
    touchElem = asElement a >>= Web.isDisplayed >> Wait.unexpected errorMsg

    handler (FailedCommand StaleElementReference _) = pure ()
    handler (FailedCommand NoSuchElement _)         = pure ()
    handler other                                   = throwM other

-- | Gets element's text when it satisfies the predicate
awaitText :: (HasElement a, MonadWebTest m, HasCallStack) => a -> (Text -> Bool) -> m Text
awaitText a f = withFrozenCallStack $ evalM $
  awaitElementWithErr (Just "having a suitable text") Nothing a (fmap f . Web.getText) >>= Web.getText
