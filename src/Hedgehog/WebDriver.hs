module Hedgehog.WebDriver
( WebContext(..)
, Millis(..)
, MonadWebTest
, WDConfig(..)
, withBrowser
)
where

import Hedgehog.WebDriver.WebContext
import Hedgehog.WebDriver.Internal.Property (finally)

import Test.WebDriver               (WDConfig, closeSession, runWD)
import Control.Concurrent.STM       (readTVarIO, newTVarIO, TVar)
import Hedgehog.Internal.Property   (PropertyT)
import Control.Monad.Reader         (runReaderT)
import Hedgehog                     (evalM, test)

import Hedgehog.Internal.Source (HasCallStack (..), withFrozenCallStack)
import Test.WebDriver.Config (mkSession, mkCaps)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Test.WebDriver.Commands      as Web

-- | Runs a new browser session in a property
withBrowser :: HasCallStack => WDConfig -> WebTest a -> PropertyT IO a
withBrowser conf m = do
  dummy <- mkSession conf
  caps <- mkCaps conf
  let ctx = WebContext dummy (Millis 10000)
  var <- liftIO (newTVarIO ctx)

  let prop = withFrozenCallStack $ evalM $ runWebTest' var (Web.createSession caps >> m)

  finally prop $ do
    ctx' <- readTVarIO var
    runWD (session ctx') Web.closeSession

runWebTest' :: TVar WebContext -> WebTest a -> PropertyT IO a
runWebTest' var (WebTest m) = test $ runReaderT m var
