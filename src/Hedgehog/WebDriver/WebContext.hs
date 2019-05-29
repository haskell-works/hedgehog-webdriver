{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Hedgehog.WebDriver.WebContext
( WebContext(..)
, Millis(..)
, withBrowser
)
where

import Control.Monad.Base          (MonadBase)
import Control.Monad.Catch         (MonadCatch, MonadThrow)
import Control.Monad.Fix           (MonadFix)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Reader        (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict  (StateT, evalStateT, get, gets, modify', runStateT)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import GHC.Generics                (Generic)
import Test.WebDriver              (WDConfig, closeSession, runWD)
import Test.WebDriver.Session      (WDSession, getSession)

import Control.Concurrent.STM


import qualified Hedgehog.WebDriver.Internal.Property as Prop

import           Hedgehog.Internal.Property
import qualified Test.WebDriver             as Web
import           Test.WebDriver.Class
import           Test.WebDriver.Config
import qualified Test.WebDriver.Session     as Web

import Control.Exception.Lifted (throwIO)
import Test.WebDriver.Internal

import Debug.Trace

newtype Millis = Millis { unMillis :: Int } deriving (Show, Eq, Ord)

data WebContext
  = WebContext
  { session :: WDSession
  , timeout :: Millis
  } deriving (Generic)

newtype WebTest a = WebTest { unWebTest :: ReaderT (TVar WebContext) (TestT IO) a }
  deriving  ( Functor, Applicative, Monad
            , MonadIO, MonadBase IO, MonadBaseControl IO
            , MonadThrow, MonadCatch
            , MonadTest)

instance Web.WDSessionState WebTest where
  getSession = WebTest $
    ask >>= liftIO . fmap session . readTVarIO
  putSession sess = WebTest $
    ask >>= liftIO . atomically . flip modifyTVar (\ctx -> ctx { session = sess })

instance WebDriver WebTest where
  doCommand method path args =
    mkRequest method path args
    >>= sendHTTPRequest
    >>= either throwIO return
    >>= getJSONResult
    >>= either throwIO return

-- | Runs a new browser session in a property
withBrowser :: WDConfig -> WebTest a -> PropertyT IO a
withBrowser conf m = do
  dummy <- mkSession conf
  caps <- mkCaps conf
  let ctx = WebContext dummy (Millis 10000)
  var <- liftIO (newTVarIO ctx)

  let prop = evalM $ runWebTest' var (Web.createSession caps >> m)

  flip Prop.finally prop $ do
    ctx' <- readTVarIO var
    Web.runWD (session ctx') Web.closeSession

runWebTest' :: TVar WebContext -> WebTest a -> PropertyT IO a
runWebTest' var (WebTest m) = test $ runReaderT m var

