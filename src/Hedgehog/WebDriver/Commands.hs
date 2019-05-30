{-# LANGUAGE OverloadedStrings #-}
module Hedgehog.WebDriver.Commands
( MonadWebTest
-- ** Awaiting for elements
, awaitElem
, awaitDisplayed, awaitNotDisplayed
, awaitEnabled, awaitDisabled

-- ** Session operations
, cleanupSession

-- ** Lower level primitives
, await
, awaitFor
)
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad (void)

import Hedgehog (MonadTest, evalM)
import Hedgehog.Internal.Source      (HasCallStack (..), withFrozenCallStack)
import Hedgehog.WebDriver.WebContext (MonadWebTest, Millis (..), WebContext (..), WebContextState (..))
import Data.Text (Text)
import Data.Bool (bool)

import           Test.WebDriver               (Element, Selector (..))
import qualified Test.WebDriver               as Web
import           Test.WebDriver.Class         (WebDriver)
import qualified Test.WebDriver.Commands      as Web
import qualified Test.WebDriver.Commands.Wait as Wait
import qualified Data.Text as Text

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

------------------------------- LOWER LEVEL PRIMITIVES ------------------------

-- | Awaits for the specified action to succeed.
--
-- It is intended to be used with WebDriver commands.
await :: (MonadWebTest m, HasCallStack) => m a -> m a
await = withFrozenCallStack $ awaitFor (pure . Right)

-- | Executes an action and awaits for the result that satisfies the predicate.
-- The predicate is expected to either return an error message, or a potentially modified result.
awaitFor :: (MonadWebTest m, HasCallStack) => (a -> m (Either Text b)) -> m a -> m b
awaitFor f ma = do
  ctx <- getWebContext
  let seconds = (fromIntegral . unMillis . timeout) ctx / 1000
  withFrozenCallStack $ evalM $ Wait.waitUntil seconds $ do
    val <- ma
    res <- f val
    case res of
      Left err -> Wait.unexpected (Text.unpack err)
      Right val' -> pure val'
