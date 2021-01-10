{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Effect
  ( sumToM
  )
where

import Data.Functor.Sum (Sum (..))


sumToM
  :: (forall x. f x -> m x)
  -> (forall x. g x -> m x)
  -> Sum f g a
  -> m a
sumToM runL runR = \case
  InL x -> runL x
  InR x -> runR x
