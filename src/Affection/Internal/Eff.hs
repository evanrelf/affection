{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Affection.Internal.Eff where

import Affection.Internal.Freer (Freer (..))
import Affection.Internal.Union (Union)


newtype Eff r a = Eff
  { runEff
      :: forall m s
       . Monad m
      => s
      -> (forall x. Union r x -> s -> m (s, x))
      -> m (s, a)
  }
  deriving (Functor, Applicative, Monad) via Freer (Union r)


liftEff :: Union r a -> Eff r a
liftEff union = Eff $ \toM -> toM union


hoistEff :: (forall x. Union r x -> Union r' x) -> Eff r a -> Eff r' a
hoistEff f eff = Eff $ \toM -> runEff eff () (toM . f)


hoistEff' :: (forall x. Union r x -> Union r' x) -> s -> Eff r a -> Eff r' (s, a)
hoistEff' f s eff = Eff $ \toM -> runEff eff (toM . f)


foldEff :: Monad m => (forall x. Union r x -> m x) -> Eff r a -> m a
foldEff toM eff = snd <$> runEff eff () toM


foldEff' :: Monad m => (forall x. Union r x -> m (s, x)) -> s -> Eff r a -> m (s, a)
foldEff' toM s eff = runEff eff s toM
