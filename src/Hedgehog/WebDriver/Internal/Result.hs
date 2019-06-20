{-# LANGUAGE DeriveAnyClass    #-}
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
import Hedgehog.Internal.Property (MonadTest, failException, failWith)
import Hedgehog.Internal.Show     (showPretty)
import Hedgehog.Internal.Source   (HasCallStack, withFrozenCallStack)
import Test.WebDriver             (FailedCommand (..))

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

evalResult :: (MonadTest m, Show e, HasCallStack) => Result e a -> m a
evalResult = \case
  Wrong x -> withFrozenCallStack $ failWith Nothing $ showPretty x
  Failure x -> withFrozenCallStack $ failException (SomeException x)
  Success x -> pure x
