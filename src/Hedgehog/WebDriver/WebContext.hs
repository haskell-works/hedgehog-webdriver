{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Hedgehog.WebDriver.WebContext
( WebContext(..)
, Millis(..)
, WebTest(..)
, WebContextState(..)
, MonadWebTest
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

import Hedgehog.Internal.Property

import           Hedgehog.Internal.Source (HasCallStack (..), withFrozenCallStack)
import qualified Test.WebDriver           as Web
import           Test.WebDriver.Class
import           Test.WebDriver.Config
import qualified Test.WebDriver.Session   as Web

import Control.Exception.Lifted (throwIO)
import Test.WebDriver.Internal

type MonadWebTest m = (MonadTest m, WebDriver m, WebContextState m, MonadCatch m)

newtype Millis = Millis { unMillis :: Int } deriving (Show, Eq, Ord)

data WebContext = WebContext
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

class WebContextState m where
  getWebContext :: m WebContext

instance WebContextState WebTest where
  getWebContext = WebTest $ ask >>= liftIO . readTVarIO
