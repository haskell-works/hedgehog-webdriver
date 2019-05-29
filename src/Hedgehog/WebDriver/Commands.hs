module Hedgehog.WebDriver.Commands
( MonadWebTest
, await
, awaitElem
, cleanupSession
)
where

import Control.Monad.Catch (MonadCatch)

import Hedgehog (MonadTest, evalM)
import Hedgehog.Internal.Source      (HasCallStack (..), withFrozenCallStack)
import Hedgehog.WebDriver.WebContext (MonadWebTest, Millis (..), WebContext (..), WebContextState (..))

import           Test.WebDriver               (Element, Selector (..))
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

-- | Awaits for the specified action to succeed.
--
-- It is intended to be used with WebDriver commands.
await :: (MonadWebTest m, HasCallStack) => m a -> m a
await ma = do
  ctx <- getWebContext
  let seconds = (fromIntegral . unMillis . timeout) ctx / 1000
  withFrozenCallStack $ evalM $ Wait.waitUntil seconds ma

-- | The same as 'Test.WebDriver.findElem', but awaits for the element
-- to appear on the web page, giving time to the page
-- and to the scripts to run.
awaitElem :: (MonadWebTest m, HasCallStack) => Selector -> m Element
awaitElem sel = withFrozenCallStack $ await (Web.findElem sel)


