{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

module Hedgehog.WebDriver.Internal.Result
where

import Control.Exception          (SomeException (..))
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import GHC.Generics               (Generic)
import Hedgehog.Internal.Property (MonadTest, failException, failWith, failDiff, diff, failure)
import Hedgehog.Internal.Show     (showPretty)
import Hedgehog.Internal.Source   (HasCallStack, withFrozenCallStack)
import Test.WebDriver             (FailedCommand (..))

import Debug.Trace

data Result e a
  = Success a
  | Wrong e
  | Failure FailedCommand
  deriving (Show, Functor, Foldable, Traversable, Generic)

isSuccess :: Result e a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

instance Applicative (Result e) where
  pure = Success
  Wrong e     <*> _ = Wrong e
  Failure cmd <*> _ = Failure cmd
  Success f   <*> a = fmap f a

instance Monad (Result a) where
  Wrong e   >>= _ = Wrong e
  Failure e >>= _ = Failure e
  Success a >>= f = f a

instance Bifunctor Result where
  bimap f _ (Wrong a)   = Wrong (f a)
  bimap _ g (Success b) = Success (g b)
  bimap _ _ (Failure e) = Failure e

instance Bifoldable Result where
  bifoldMap f _ (Wrong a)   = f a
  bifoldMap _ _ (Failure _) = mempty
  bifoldMap _ g (Success b) = g b

instance Bitraversable Result where
  bitraverse f _ (Wrong a)   = Wrong <$> f a
  bitraverse f _ (Failure e) = pure (Failure e)
  bitraverse _ g (Success b) = Success <$> g b

instance Semigroup (Result a b) where
  Wrong _ <> b    = b
  Failure _ <> b  = b
  _ <> b          = b

resultToMaybe :: Result e a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe _           = Nothing

evalResult :: (MonadTest m, Show e, HasCallStack) => Result e a -> m a
evalResult = \case
  Wrong x -> -- withFrozenCallStack $
    trace ("WRONG RESULT: " <> showPretty x <> "\n\n\n\n") $
      failWith Nothing $ "WRONG RESULT: " <> showPretty x
  Failure x -> -- withFrozenCallStack $
    trace ("EXCEPTION: " <> show x <> "\n\n\n\n") $
      failException (SomeException x)
  Success x -> pure x

evalExpectedResult :: (MonadTest m, Show e, Show a, HasCallStack) => a -> Result e a -> m a
evalExpectedResult a = \case
  Wrong x -> -- withFrozenCallStack $
    trace ("WRONG RESULT: " <> showPretty x <> "\n\n\n\n") $
      failDiff x a >> failure
  Failure x -> -- withFrozenCallStack $
    trace ("EXCEPTION: " <> show x <> "\n\n\n\n") $
      failException (SomeException x)
  Success x -> pure x
