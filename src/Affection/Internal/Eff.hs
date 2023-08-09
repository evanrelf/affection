{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Affection.Internal.Eff where

import Affection.Internal.Freer (Freer (..))
import Affection.Internal.Union (Union)


newtype Eff r a = Eff
  { runEff
      :: forall m
       . Monad m
      => (forall x. Union r x -> m x)
      -> m a
  }
  deriving (Functor, Applicative, Monad) via Freer (Union r)


liftEff :: Union r a -> Eff r a
liftEff union = Eff $ \toM -> toM union


hoistEff :: (forall x. Union r x -> Union r' x) -> Eff r a -> Eff r' a
hoistEff f eff = Eff $ \toM -> runEff eff (toM . f)


foldEff :: Monad m => (forall x. Union r x -> m x) -> Eff r a -> m a
foldEff toM eff = runEff eff toM
