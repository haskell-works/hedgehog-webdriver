module Hedgehog.WebDriver.Internal.Property
where

import Control.Monad.Trans        (lift)
import Hedgehog.Internal.Gen      (GenT)
import Hedgehog.Internal.Property

propFinally :: Property -> IO () -> Property
propFinally prop f = do
  let test = propertyTest prop
  prop { propertyTest = test `finally` f }

-- | Resource bracket for 'PropertyT'
bracket :: Monad m
  => m a                    -- ^ Allocate resource
  -> (a -> m ())            -- ^ Destroy the resource
  -> (a -> PropertyT m b)   -- ^ Use the resource
  -> PropertyT m b
bracket create destroy f = do
  resource <- liftProp create
  finally (f resource) (destroy resource)

-- | Run an action when the property is finished, either successfully or not
finally :: Monad m
  => PropertyT m a
  -> m ()           -- ^ Action to execute when property has finished (or failed)
  -> PropertyT m a
finally prop f = do
  let res = runTestT $ unPropertyT prop
  PropertyT $ mkTestT $ res >>= (\a -> lift f >> pure a)

-- | Helper function: lifting any monadic action to `PropertyT`
liftProp :: Monad m => m a -> PropertyT m a
liftProp f = PropertyT $ mkTestT $ lift (f >>= (\a -> pure (Right a, mempty)))
