{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Internal.Eff where

import Effect.Internal.Freer (Freer, foldFreer, hoistFreer, liftFreer)
import Effect.Internal.OpenUnion (Member (..), Union, decompose, extract)


newtype Eff es a = Eff (Freer (Union es) a)
  deriving newtype (Functor, Applicative, Monad)


send :: Member e es => e a -> Eff es a
send = Eff . liftFreer . inject


interpret
  :: forall e1 e2 es a
   . Member e2 es
  => (forall x. e1 x -> e2 x)
  -> Eff (e1 ': es) a
  -> Eff es a
interpret handler (Eff freer) = Eff (hoistFreer f freer)
  where
  f :: forall x. Union (e1 ': es) x -> Union es x
  f union =
    case decompose union of
      Left union' -> union'
      Right e1 -> inject (handler e1)


runM :: Monad m => Eff '[m] a -> m a
runM (Eff freer) = foldFreer extract freer
