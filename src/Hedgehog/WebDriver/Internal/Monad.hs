{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hedgehog.WebDriver.Internal.Monad
where

import Control.Monad.IO.Class        (MonadIO)
import Control.Monad.Reader          (MonadReader)
import Hedgehog                      (MonadTest)
import Hedgehog.WebDriver.WebContext (WebContext)

type MonadWebTest m = (MonadIO m, MonadTest m, MonadReader WebContext m)
