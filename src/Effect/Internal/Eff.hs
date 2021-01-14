{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Internal.Eff where

import Effect.Internal.Freer (Freer (..), foldFreer, hoistFreer, liftFreer)
import Effect.Internal.OpenUnion (Member (..), Union, decompose, extract)


newtype Eff r a = Eff { runEff :: Freer (Union r) a }
  deriving newtype (Functor, Applicative, Monad)


send :: Member e r => e a -> Eff r a
send = Eff . liftFreer . inject


interpret
  :: forall e1 e2 r a
   . Member e2 r
  => (forall x. e1 x -> e2 x)
  -> Eff (e1 ': r) a
  -> Eff r a
interpret handler (Eff freer) = Eff (hoistFreer f freer)
  where
  f :: forall x. Union (e1 ': r) x -> Union r x
  f union =
    case decompose union of
      Left union' -> union'
      Right e1 -> inject (handler e1)


-- interpret2
--   :: forall e r a
--    . (forall x. e x -> Eff r x)
--   -> Eff (e ': r) a
--   -> Eff r a
-- interpret2 handler (Eff freer) = undefined
--   where
--   pop :: (forall x. e x -> Eff r x) -> Union (e ': r) a -> Freer (Union r) a
--   pop handler union =
--     case decompose union of
--       Left union' -> liftFreer union'
--       Right e -> runEff $ handler e


runM :: Monad m => Eff '[m] a -> m a
runM (Eff freer) = foldFreer extract freer
