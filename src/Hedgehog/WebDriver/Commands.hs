{-# LANGUAGE OverloadedStrings #-}
module Hedgehog.WebDriver.Commands
( MonadWebTest
-- ** Awaiting for elements
, awaitElem
, awaitElemText
, awaitDisplayed, awaitNotDisplayed
, awaitEnabled, awaitDisabled
, awaitDisappear

-- ** Session operations
, cleanupSession

-- ** Lower level primitives
, await
, awaitFor
)
where

import Control.Monad       (void)
import Control.Monad.Catch (MonadCatch, catch, throwM)

import Data.Bool                         (bool)
import Data.Either                       (isRight)
import Data.Text                         (Text)
import Hedgehog                          (MonadTest, diff, evalEither, evalM)
import Hedgehog.Internal.Source          (HasCallStack (..), withFrozenCallStack)
import Hedgehog.WebDriver.Internal.Retry (retrying, retryingMap)
import Hedgehog.WebDriver.WebContext     (Millis (..), MonadWebTest, WebContext (..), WebContextState (..))

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
awaitElem :: (MonadWebTest m, HasCallStack) => Selector -> m Element
awaitElem sel = withFrozenCallStack $ await (Web.findElem sel)

-- | Awaits for the element to be visible of the web page
awaitDisplayed :: (MonadWebTest m, HasCallStack) => Element -> m ()
awaitDisplayed elem =
  let errorMsg = "Element is not displayed"
  in withFrozenCallStack $ awaitFor (pure . bool (Left errorMsg) (Right ())) (Web.isDisplayed elem)

-- | Awaits for the element to be not visible on the web page
awaitNotDisplayed :: (MonadWebTest m, HasCallStack) => Element -> m ()
awaitNotDisplayed elem =
  let errorMsg = "Element es expected to be hidden, but it is still visible"
  in withFrozenCallStack $ awaitFor (pure . bool (Left errorMsg) (Right ())) (not <$> Web.isDisplayed elem)

-- | Awaits for the element to be enabled
awaitEnabled :: (MonadWebTest m, HasCallStack) => Element -> m ()
awaitEnabled elem =
  let errorMsg = "Element is not displayed"
  in withFrozenCallStack $ awaitFor (pure . bool (Left errorMsg) (Right ())) (Web.isEnabled elem)

-- | Awaits for the element to be disabled
awaitDisabled :: (MonadWebTest m, HasCallStack) => Element -> m ()
awaitDisabled elem =
  let errorMsg = "Element is not displayed"
  in withFrozenCallStack $ awaitFor (pure . bool (Left errorMsg) (Right ())) (not <$> Web.isEnabled elem)

awaitDisappear :: (MonadWebTest m, HasCallStack) => Element -> m ()
awaitDisappear elem =
  withFrozenCallStack $ await (touchElem `catch` handler)
  where
    errorMsg = "Element is expected to disappear from DOM, but it is still there"
    touchElem = Web.isDisplayed elem >> Wait.unexpected errorMsg

    handler (FailedCommand StaleElementReference _) = pure ()
    handler other                                   = throwM other

awaitElemText :: (MonadWebTest m, HasCallStack) => Element -> (Text -> Bool) -> m Text
awaitElemText elem f = withFrozenCallStack $ evalM $
  retrying (pure . f) (Web.getText elem) >>= evalEither
------------------------------- LOWER LEVEL PRIMITIVES ------------------------

-- | Awaits for the specified action to succeed.
--
-- It is intended to be used with WebDriver commands.
await :: (MonadWebTest m, HasCallStack) => m a -> m a
await = withFrozenCallStack $ awaitFor (pure . Right)

-- | Executes an action and awaits for the result that satisfies the predicate.
-- The predicate is expected to either return an error message, or a potentially modified result.
awaitFor :: (MonadWebTest m, HasCallStack) => (a -> m (Either Text b)) -> m a -> m b
awaitFor f ma = withFrozenCallStack $ evalM $ do
  vals <- retryingMap (fmap (either (const Nothing) Just) . f) ma
  case vals of
    Right b -> pure b
    Left a  -> f a >>= evalEither


awaitDiff :: (Show a, Eq a, MonadWebTest m, HasCallStack) => m a -> (a -> a -> Bool) -> a -> m ()
awaitDiff ma f a = withFrozenCallStack $ do
  res <- retrying (\x -> pure (f x a)) ma
  diff (either id id res) f a
