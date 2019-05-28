{-# LANGUAGE DeriveGeneric #-}
module Hedgehog.WebDriver.WebContext
( WebContext(..)
, Millis(..)
, withSession
)
where

import Control.Monad.Base          (MonadBase)
import Control.Monad.Fix           (MonadFix)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Reader        (ReaderT, runReaderT)
import Control.Monad.State.Strict  (StateT, gets, modify', runStateT)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import GHC.Generics                (Generic)
import Hedgehog.Internal.Property  (PropertyT)
import Test.WebDriver              (WDConfig, closeSession, runSession, runWD)
import Test.WebDriver.Session      (WDSession, getSession)

import qualified Hedgehog.WebDriver.Internal.Property as Prop

newtype Millis = Millis { unMillis :: Int } deriving (Show, Eq, Ord)

data WebContext
  = WebContext
  { session :: WDSession
  , timeout :: Millis
  } deriving (Generic)

-- | Runs a new browser session in a property
withSession :: WDConfig -> ReaderT WebContext (PropertyT IO) a -> PropertyT IO a
withSession cfg f = do
  ctx <- liftIO mkSession
  Prop.finally (liftIO $ killSession ctx) (runReaderT f ctx)
  where
    mkSession = runSession cfg $ flip WebContext (Millis 10000) <$> getSession
    killSession ctx = runWD (session ctx) closeSession
