{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Affection.Lift
  ( Lift (..)
  , lift
  , runM
  )
where

import Affection (Eff, Member, send)
import Affection.Internal.Eff (foldEff)
import Affection.Internal.Union (extract)
import Control.Monad.IO.Class (MonadIO (..))


data Lift m a where
  Lift :: m a -> Lift m a


lift :: Member (Lift m) r => m a -> Eff r a
lift m = send (Lift m)


instance Member (Lift IO) r => MonadIO (Eff r) where
  liftIO io = send (Lift io)


runM :: forall m a. Monad m => Eff '[Lift m] a -> m a
runM eff = foldEff ((\(Lift m) -> m) . extract) eff
