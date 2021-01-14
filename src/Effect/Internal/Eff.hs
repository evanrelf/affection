{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Effect.Internal.Eff where

import Effect.Internal.OpenUnion (Union)


newtype Eff r a = Eff
  { runEff
      :: forall m
       . Monad m
      => (forall x. Union r (Eff r) x -> m x)
      -> m a
  }


instance Functor (Eff r) where
  fmap :: (a -> b) -> Eff r a -> Eff r b
  fmap a2b eff = Eff \toM ->
    let
      x = runEff eff toM
    in
      fmap a2b x


instance Applicative (Eff r) where
  pure :: a -> Eff r a
  pure x = Eff \_ -> pure x

  (<*>) :: Eff f (a -> b) -> Eff f a -> Eff f b
  (<*>) effF effX = Eff \toM ->
    let
      f = runEff effF toM
      x = runEff effX toM
    in
      f <*> x


instance Monad (Eff r) where
  (>>=) :: Eff r a -> (a -> Eff r b) -> Eff r b
  (>>=) eff a2Eb = Eff \toM ->
    let
      m = runEff eff toM
      k x = runEff (a2Eb x) toM
    in
      m >>= k


liftEff :: Union r (Eff r) a -> Eff r a
liftEff union = Eff \toM -> toM union


hoistEff
  :: (forall x. Union r (Eff r) x -> Union r' (Eff r') x)
  -> Eff r a
  -> Eff r' a
hoistEff f eff = Eff \toM -> runEff eff (toM . f)


foldEff :: Monad m => (forall x. Union r (Eff r) x -> m x) -> Eff r a -> m a
foldEff toM eff = runEff eff toM
