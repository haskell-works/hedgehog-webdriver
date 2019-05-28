{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Hedgehog.WebDriver.Commands.Result
where

import Control.Exception      (SomeException)
import GHC.Generics           (Generic)
import Hedgehog.Internal.Property
import Hedgehog.Internal.Show
import Hedgehog
import Hedgehog.Internal.Source   (HasCallStack (..), withFrozenCallStack)
import Data.Text (Text)

import qualified Data.Text as Text

data Result a
  = Exception SomeException
  | UnexpectedValue a
  | Value a
  deriving (Show, Generic, Functor)

successResult :: Result a -> Bool
successResult (Value _) = True
successResult _         = False
{-# INLINE successResult #-}

evalResult :: (MonadTest m, Show a, HasCallStack) => Maybe a -> Result a -> m a
evalResult expected res = case (expected, res) of
  (_, Exception ex) -> withFrozenCallStack $ failException ex
  (Just a, UnexpectedValue err) -> withFrozenCallStack $ failDiff err a >> failWith Nothing "Failed"
  (Nothing, UnexpectedValue err) -> withFrozenCallStack $
    failWith Nothing $ unlines
      [ "Failed"
      , "━━ Unexpected value ━━"
      , showPretty err
      ]
  (_, Value a) -> pure a

evalResultMsg :: (MonadTest m, Show a, HasCallStack) => Text -> Result a -> m a
evalResultMsg msg res = case res of
  Exception ex -> withFrozenCallStack $ failException ex
  Value a -> pure a
  UnexpectedValue _ -> failWith Nothing (Text.unpack msg)
