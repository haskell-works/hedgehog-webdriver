-- | This module provides machinery for accessing web elements "reliably".
--
-- It has similar functionality to what 'Test.WebDriver.Commands.Wait'
-- provides, but allows providing better error messaging.
module Hedgehog.WebDriver.Internal.Retry
where

import Control.Exception                  (Exception)
import Control.Monad.Catch                (MonadCatch, MonadThrow, catch, throwM)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bool                          (bool)
import Data.IORef                         (atomicWriteIORef, newIORef, readIORef)
import Data.Maybe                         (isJust, isNothing)
import Data.Text                          (Text)
import Hedgehog.WebDriver.Internal.Result
import Hedgehog.WebDriver.WebContext
import Test.WebDriver                     (FailedCommand (..))

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Control.Retry as Retry


-- | Retries a given action until a given predicate is satisfied
-- or the timeout is reached.
-- Returns either the "correct" value that satisfied the predicate,
-- or the "wrong" value that failed the predicate.
-- The wrong value can be used then in error messages.
retrying :: (WebContextState m, MonadIO m, MonadCatch m)
  => (a -> m Bool)
  -> m a
  -> m (Result a a)
retrying f =
  retryingMap (\a -> bool Nothing (Just a) <$> f a)

-- | Like 'retrying' but allows keeping some state 's' between retries.
retryingS :: (WebContextState m, MonadIO m, MonadCatch m)
  => (s -> a -> m (s, Bool))
  -> s
  -> m a
  -> m (Result a a)
retryingS f z =
  retryingMapS (\s a -> fmap (bool Nothing (Just a)) <$> f s a) z

-- | Retries a given action until a given transformation is successful
-- or the timeout is reached.
-- Returns either the "correct" value that satisfied the predicate,
-- or the "wrong" value that failed the transformation.
-- The wrong value can be used then in error messages.
retryingMap :: (WebContextState m, MonadIO m, MonadCatch m)
  => (a -> m (Maybe b))
  -> m a
  -> m (Result a b)
retryingMap f ma = do
  ctx <- getWebContext
  retryingMap' (timeout ctx) f ma

-- | Like 'retryingMap' but allows keeping some state 's' between retries.
retryingMapS :: (WebContextState m, MonadIO m, MonadCatch m)
  => (s -> a -> m (s, Maybe b))
  -> s
  -> m a
  -> m (Result a b)
retryingMapS f z ma = do
  ctx <- getWebContext
  retryingMapS' (timeout ctx) f z ma

retryingMap' :: (MonadIO m, MonadCatch m)
  => Millis                     -- ^ Keep trying for that long
  -> (a -> m (Maybe b))         -- ^ Predicate
  -> m a                        -- ^ Action that returns value
  -> m (Result a b)             -- ^ Result
retryingMap' (Millis ms) f ma = do
  let policy = Retry.limitRetriesByCumulativeDelay (ms * 1000) $ Retry.constantDelay 50000
  let ma' = (Right <$> ma) `catch` handler
  lastA <- Retry.retrying policy (const check) (const ma')
  case lastA of
    Left err -> pure (Failure err)
    Right a' -> maybe (Wrong a') Success <$> f a'

  where
    handler err@(FailedCommand _ _) = pure (Left err)
    check = either (const $ pure True) (fmap isNothing <$> f)

retryingMapS' :: (WebContextState m, MonadIO m, MonadCatch m)
  => Millis
  -> (s -> a -> m (s, Maybe b))
  -> s
  -> m a
  -> m (Result a b)
retryingMapS' (Millis ms) f z ma = do
  cell <- liftIO (newIORef z)
  let policy = Retry.limitRetriesByCumulativeDelay (ms * 1000) $ Retry.constantDelay 50000
  let ma' = (Right <$> ma) `catch` handler
  lastA <- Retry.retrying policy (const (check cell)) (const ma')
  case lastA of
    Left err -> pure (Failure err)
    Right a' -> do
      s <- liftIO (readIORef cell)
      (_, mbRes) <- f s a'
      pure $ maybe (Wrong a') Success mbRes

  where
    handler err@(FailedCommand _ _) = pure (Left err)
    check cell = either (const $ pure True) $ \a -> do --(fmap isNothing <$> f)
      s <- liftIO (readIORef cell)
      (s', res) <- f s a
      case res of
        Nothing -> liftIO (atomicWriteIORef cell s') >> pure True
        Just b  -> pure False
