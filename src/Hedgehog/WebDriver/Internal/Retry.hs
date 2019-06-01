{-# LANGUAGE LambdaCase #-}

-- | This module provides machinery for accessing web elements "reliably".
--
-- It has similar functionality to what 'Test.WebDriver.Commands.Wait'
-- provides, but allows providing better error messaging.
module Hedgehog.WebDriver.Internal.Retry
where

import Control.Exception             (Exception)
import Control.Monad.Catch           (MonadCatch, MonadThrow, catch, throwM)
import Data.Bool                     (bool)
import Data.Maybe                    (isJust, isNothing)
import Data.Text                     (Text)
import Hedgehog.WebDriver.WebContext
import Test.WebDriver                (FailedCommand (..))

import Control.Monad.IO.Class

import qualified Control.Retry as Retry

-- | Retries a given action until a given predicate is satisfied
-- or the timeout is reached.
-- Returns either the "correct" value that satisfied the predicate,
-- or the "wrong" value that failed the predicate.
-- The wrong value can be used then in error messages.
retrying :: (WebContextState m, MonadIO m, MonadCatch m)
  => (a -> m Bool)
  -> m a
  -> m (Either a a)
retrying f =
  retryingMap (\a -> bool Nothing (Just a) <$> f a)

-- | Retries a given action until a given transformation is successful
-- or the timeout is reached.
-- Returns either the "correct" value that satisfied the predicate,
-- or the "wrong" value that failed the transformation.
-- The wrong value can be used then in error messages.
retryingMap :: (WebContextState m, MonadIO m, MonadCatch m)
  => (a -> m (Maybe b))
  -> m a
  -> m (Either a b)
retryingMap f ma = do
  ctx <- getWebContext
  retryingMap' (timeout ctx) f ma

retryingMap' :: (MonadIO m, MonadThrow m, MonadCatch m)
  => Millis             -- ^ Keep trying for that long
  -> (a -> m (Maybe b)) -- ^ Predicate
  -> m a                -- ^ Action that returns value
  -> m (Either a b)     -- ^ Result
retryingMap' (Millis ms) f ma = do
  let policy = Retry.limitRetriesByCumulativeDelay (ms * 1000) $ Retry.constantDelay 50000
  let ma' = (Right <$> ma) `catch` handler
  lastA <- Retry.retrying policy (const check) (const ma')
  case lastA of
    Left err -> throwM err
    Right a' -> maybe (Left a') Right <$> f a'

  where
    handler err@(FailedCommand _ _) = pure (Left err)
    check a =
      either (const $ pure True) (fmap isNothing <$> f) a

