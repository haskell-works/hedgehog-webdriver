{-# LANGUAGE OverloadedStrings #-}
module Hedgehog.WebDriver.Commands
( MonadWebTest
, Selector(..)
, Element
-- ** Reliable operations on Element
, awaitElem, awaitElemWithin
, awaitElemWithin'
, awaitText
, awaitDisplayed, awaitNotDisplayed
, awaitEnabled, awaitDisabled
, awaitMissing
, awaitDiff

-- ** Lower level primitives
, MonadWebDriver
, HasElement
, findAll

-- ** General purpose helpers
-- | These functions are not intended for causal use, however can play nice
-- as a compromise between complexity and good failure messages.
, await, awaitMaybe
, awaitUntil, awaitUntilS
, awaitUntilM, awaitUntilMS
, awaitMaybeUntil, awaitMaybeUntilS
, awaitMaybeUntilM, awaitMaybeUntilMS

-- ** Lower-level primitives
, retrying, retryingS
, retryingMap, retryingMapS
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
import Hedgehog                             (MonadTest, diff, evalEither, evalM, (===), diff)
import Hedgehog.Internal.Property           (failWith)
import Hedgehog.Internal.Show               (showPretty)
import Hedgehog.Internal.Source             (HasCallStack (..), withFrozenCallStack)
import Hedgehog.WebDriver.Internal.Commands
import Hedgehog.WebDriver.Internal.Result   (Result (..), evalResult, evalExpectedResult, resultToMaybe)
import Hedgehog.WebDriver.Internal.Retry    (retrying, retryingS, retryingMap, retryingMapS)
import Hedgehog.WebDriver.WebContext        (Millis (..), MonadWebDriver, MonadWebTest, WebContext (..), WebContextState (..))

import qualified Data.List.NonEmpty           as Nel
import qualified Data.Text                    as Text
import           Test.WebDriver               (Element, FailedCommand (..), FailedCommandType (..), Selector (..))
import qualified Test.WebDriver               as Web
import           Test.WebDriver.Class         (WebDriver)
import qualified Test.WebDriver.Commands      as Web
import qualified Test.WebDriver.Commands.Wait as Wait

-- | Fails the test if the two arguments provided are not equal.
-- Retries comparison until it is successful or until the timeout is reached.
-- (====) :: (HasElement a, MonadWebTest m, HasCallStack) => a -> Text -> m ()
-- (====) a txt = withFrozenCallStack $ evalM $ do
--   res <- retrying (pure . (== txt)) (asElement a >>= Web.getText)
--   case res of
--     Right val -> pure ()
--     Left val  -> val === txt

-- (/====) :: (HasElement a, MonadWebTest m, HasCallStack) => a -> Text -> m ()
-- (/====) a txt = withFrozenCallStack $ evalM $ do
--   res <- retrying (pure . (/= txt)) (asElement a >>= Web.getText)
--   case res of
--     Right val -> pure ()
--     Left val  -> val /=== txt

-- (===~) :: (HasElement a, MonadWebTest m, HasCallStack) => a -> (Text -> Bool) -> m ()
-- (===~) a f = withFrozenCallStack $ void $ awaitUntil f (asElement a >>= Web.getText)

-- | Same as 'Test.WebDriver.findElem', but awaits until the element is found
-- or a timeout is reached.
awaitElem :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitElem a = -- withFrozenCallStack $
  awaitElemWithin' Nothing a (pure . const True)

-- | Same as 'Test.WebDriver.findElemWithin', but awaits until the element is found
-- or a timeout is reached.
awaitElemWithin :: (HasElement a, MonadWebTest m, HasCallStack) => Maybe Element -> a -> m Element
awaitElemWithin root a = -- withFrozenCallStack $
  awaitElemWithin' root a (pure . const True)

-- | Awaits for an element that is visible on the page
awaitDisplayed :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitDisplayed a = -- withFrozenCallStack $
  awaitElementWithErr (Just "displayed") Nothing a Web.isDisplayed

awaitNotDisplayed :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitNotDisplayed a = -- withFrozenCallStack $
  awaitElementWithErr (Just "not displayed") Nothing a (fmap not . Web.isDisplayed)

-- | Awaits for the element to be enabled
awaitEnabled :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitEnabled a = -- withFrozenCallStack $
  awaitElementWithErr (Just "enabled") Nothing a Web.isEnabled

-- | Awaits for the element to be disabled
awaitDisabled :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m Element
awaitDisabled a = -- withFrozenCallStack $
  awaitElementWithErr (Just "disabled") Nothing a (fmap not . Web.isEnabled)

awaitMissing :: (HasElement a, MonadWebTest m, HasCallStack) => a -> m ()
awaitMissing a = -- withFrozenCallStack $
  retrying (pure . const True) (touchElem `catch` handler)
    <&> bimap (const errorMsg) id
    >>= evalResult
  where
    errorMsg = "Element is expected to disappear from DOM, but it is still there"
    touchElem = asElement a >>= Web.isDisplayed >> Wait.unexpected errorMsg

    handler (FailedCommand StaleElementReference _) = pure ()
    handler (FailedCommand NoSuchElement _)         = pure ()
    handler other                                   = throwM other

-- | Gets element's text when it satisfies the predicate
awaitText :: (HasElement a, MonadWebTest m, HasCallStack) => a -> (Text -> Bool) -> m Text
awaitText a f = -- withFrozenCallStack $
  awaitElementWithErr (Just "having a suitable text") Nothing a (fmap f . Web.getText) >>= Web.getText

-- | Any result returned by the computation is good, but if the computation
-- fails then it will be retried until it succeeds or the timeout is reached.
await :: (Show a, MonadWebTest m) => m a -> m a
await ma = -- withFrozenCallStack $
  retrying (pure . const True) ma >>= evalResult

awaitMaybe :: MonadWebTest m => m a -> m (Maybe a)
awaitMaybe =
  fmap resultToMaybe . retrying (pure . const True)

awaitUntil :: (Show a, MonadWebTest m) => (a -> Bool) -> m a -> m a
awaitUntil f ma = --withFrozenCallStack .
  retrying (pure . f) ma >>= evalResult

awaitUntilS :: (Show a, MonadWebTest m) => (s -> a -> (s, Bool)) -> s -> m a -> m a
awaitUntilS f z ma =
  retryingS (\s -> pure . f s) z ma >>= evalResult

awaitMaybeUntil :: MonadWebTest m => (a -> Bool) -> m a -> m (Maybe a)
awaitMaybeUntil f =
  fmap resultToMaybe . retrying (pure . f)

awaitMaybeUntilS :: MonadWebTest m => (s -> a -> (s, Bool)) -> s -> m a -> m (Maybe a)
awaitMaybeUntilS f z =
  fmap resultToMaybe . retryingS (\s -> pure . f s) z

awaitUntilM :: (Show a, MonadWebTest m) => (a -> m Bool) -> m a -> m a
awaitUntilM f ma = --withFrozenCallStack $
  retrying f ma >>= evalResult

awaitUntilMS :: (Show a, MonadWebTest m) => (s -> a -> m (s, Bool)) -> s -> m a -> m a
awaitUntilMS f z ma = --withFrozenCallStack $
  retryingS f z ma >>= evalResult

awaitMaybeUntilM :: MonadWebTest m => (a -> m Bool) -> m a -> m (Maybe a)
awaitMaybeUntilM f = fmap resultToMaybe . retrying f

awaitMaybeUntilMS :: MonadWebTest m => (s -> a -> m (s, Bool)) -> s -> m a -> m (Maybe a)
awaitMaybeUntilMS f z = fmap resultToMaybe . retryingS f z

awaitDiff :: (Show a, MonadWebTest m) => a -> (a -> a -> Bool) -> m a -> m a
awaitDiff a f ma =
  retrying (pure . f a) ma >>= evalExpectedResult a

------------------------------ INTERNAL HELPERS -------------------------------
awaitElemWithin' :: (HasElement a, MonadWebTest m, HasCallStack)
  => Maybe Element
  -> a
  -> (Element -> m Bool)
  -> m Element
awaitElemWithin' = -- withFrozenCallStack $
  awaitElementWithErr Nothing
