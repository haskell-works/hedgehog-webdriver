module Hedgehog.WebDriver.Internal.Commands
where

import Control.Monad                     (filterM)
import Data.Bifunctor                    (bimap)
import Data.Functor                      ((<&>))
import Data.List.NonEmpty                (NonEmpty, nonEmpty)
import Data.Maybe                        (fromMaybe)
import Hedgehog                          (evalEither, evalM)
import Hedgehog.Internal.Show            (showPretty)
import Hedgehog.Internal.Source          (HasCallStack (..), withFrozenCallStack)
import Hedgehog.WebDriver.Internal.Retry (retrying, retryingMap)
import Hedgehog.WebDriver.WebContext     (MonadWebDriver, MonadWebTest)
import Test.WebDriver                    (Element, FailedCommand (..), FailedCommandType (..), Selector (..))

import qualified Data.List.NonEmpty as Nel
import qualified Test.WebDriver     as Web

class HasElement a where
  asElement :: MonadWebDriver m => a -> m Element
  getElements :: MonadWebDriver m
    => a
    -> Maybe Element
    -> (Element -> m Bool)
    -> m (Either [Element] (NonEmpty Element))
  toString :: a -> String

instance HasElement Element where
  asElement = pure
  getElements elem _ f = retrying f (pure elem) <&> bimap pure pure
  toString elem = "<element>"

instance HasElement Selector where
  asElement = Web.findElem
  getElements = findAll
  toString = showPretty

-- | Get all teh element for a given selector which satisfy the predicate.
-- The search will be retried until at least one element is found or a timeot is reached.
findAll :: MonadWebDriver m
  => Selector                                 -- ^ Selector
  -> Maybe Element                            -- ^ Search context, an element to search within
  -> (Element -> m Bool)                      -- ^ Search predicate
  -> m (Either [Element] (NonEmpty Element))
findAll sel root f =
  let finder = maybe Web.findElems Web.findElemsFrom root
  in retryingMap (fmap nonEmpty . filterM f) (finder sel)

--------------------------------- INTERNAL ? ----------------------------------

awaitElementWithErr :: (HasElement a, MonadWebTest m, HasCallStack)
  => Maybe String         -- ^ A part of the error message, describing the predicate
  -> Maybe Element        -- ^ A root element to search within
  -> a                    -- ^ What to look for (selector or element)
  -> (Element -> m Bool)  -- ^ A predicate to verify if the search was successfil
  -> m Element
awaitElementWithErr err root a f = withFrozenCallStack $ evalM $
  getElements a root f
    <&> bimap errorMsg Nel.head
    >>= evalEither
  where
    errorMsg as = unlines
      [ "For a input: " <> toString a
      , "Found " <> showPretty (length as) <> " elements"
      , "And none of them was " <> fromMaybe "satisfying a given predicate" err
      ]

