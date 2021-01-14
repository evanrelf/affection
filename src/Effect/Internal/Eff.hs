{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Effect.Internal.Eff where

import Effect.Internal.Freer (Freer (..))
import Effect.Internal.OpenUnion (Union)


newtype Eff r a = Eff
  { runEff
      :: forall m
       . Monad m
      => (forall x. Union r (Eff r) x -> m x)
      -> m a
  } deriving (Functor, Applicative, Monad) via Freer (Union r (Eff r))


liftEff :: Union r (Eff r) a -> Eff r a
liftEff union = Eff $ \toM -> toM union


hoistEff
  :: (forall x. Union r (Eff r) x -> Union r' (Eff r') x)
  -> Eff r a
  -> Eff r' a
hoistEff f eff = Eff $ \toM -> runEff eff (toM . f)


foldEff :: Monad m => (forall x. Union r (Eff r) x -> m x) -> Eff r a -> m a
foldEff toM eff = runEff eff toM
