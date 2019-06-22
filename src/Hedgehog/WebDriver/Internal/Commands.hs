module Hedgehog.WebDriver.Internal.Commands
where

import Control.Exception                  (SomeException (..))
import Control.Monad                      (filterM)
import Data.Bifunctor                     (bimap)
import Data.Functor                       ((<&>))
import Data.List.NonEmpty                 (NonEmpty, nonEmpty)
import Data.Maybe                         (fromMaybe)
import Hedgehog                           (evalEither, evalM)
import Hedgehog.Internal.Property         (failException, failWith)
import Hedgehog.Internal.Show             (showPretty)
import Hedgehog.Internal.Source           (HasCallStack (..), withFrozenCallStack)
import Hedgehog.WebDriver.Internal.Result (Result (..))
import Hedgehog.WebDriver.Internal.Retry  (retrying, retryingMap)
import Hedgehog.WebDriver.WebContext      (MonadWebDriver, MonadWebTest)
import Test.WebDriver                     (Element, FailedCommand (..), FailedCommandType (..), Selector (..))

import qualified Data.List.NonEmpty as Nel
import qualified Test.WebDriver     as Web

-- | 'HasElement' unifies operations on 'Selector' and 'Element'
class HasElement a where
  -- | Convert a value into an element
  asElement :: MonadWebDriver m => a -> m Element

  -- | Get all elements corresponding to a given value.
  -- For 'Selector' there can be many, for an 'Element' there will only be one.
  getElements :: MonadWebDriver m
    => a
    -> Maybe Element
    -> (Element -> m Bool)
    -> m (Result [Element] (NonEmpty Element))

  -- | Determines value's string representation.
  -- The intent of this function is to be used in an error / test failure messages.
  toString :: a -> String

instance HasElement Element where
  asElement = pure
  getElements elem _ f = retrying f (pure elem) <&> bimap pure pure
  toString elem = "<element>"

instance HasElement Selector where
  asElement = Web.findElem
  getElements = findAll
  toString = showPretty

-- | Find all elemens for a given selector which satisfy the predicate.
-- The search will be retried until at least one element is found or a timeot is reached.
--
-- When 'findAll' is unable to get a non empty list of elements
-- that satisfy the predicate, it returns what it was able to find
-- using the 'Left' constructor.
findAll :: MonadWebDriver m
  => Selector                                 -- ^ Selector
  -> Maybe Element                            -- ^ Search context, an element to search within
  -> (Element -> m Bool)                      -- ^ Search predicate
  -> m (Result [Element] (NonEmpty Element))
findAll sel root f =
  let finder = maybe Web.findElems Web.findElemsFrom root
  in retryingMap (fmap nonEmpty . filterM f) (finder sel)

--------------------------------- INTERNAL ? ----------------------------------

awaitElementWithErr :: (HasElement a, MonadWebTest m)
  => Maybe String         -- ^ A part of the error message, describing the predicate
  -> Maybe Element        -- ^ A root element to search within
  -> a                    -- ^ What to look for (selector or element)
  -> (Element -> m Bool)  -- ^ A predicate to verify if the search was successfil
  -> m Element
awaitElementWithErr err root a f =
  do
    elems <- getElements a root f
    case elems of
      Failure err -> failException (SomeException err)
      Success vals -> pure (Nel.head vals)
      Wrong wrong ->
        failWith Nothing $ unlines $
          [ "Failed"
          , "━━ expected ━━"
          , toString a
          , "━━ actual ━━"
          , showPretty (length wrong) <> " elements"
          , "And none of them was " <> fromMaybe "satisfying a given predicate" err
          ]
