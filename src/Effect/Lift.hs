{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Lift
  ( Lift
  , lift
  , runLift
  , runM
  )
where

import Effect (Member, interpret, send)
import Effect.Internal.Eff (Eff (..))
import Effect.Internal.Freer (foldFreer)
import Effect.Internal.OpenUnion (extract)


data Lift m a where
  Lift :: Monad m => m a -> Lift m a


lift :: (Monad m, Member (Lift m) r) => m a -> Eff r a
lift m = send $ Lift m


liftToM :: Lift m a -> m a
liftToM = \case
  Lift m -> m


runLift :: Member m r => Eff (Lift m ': r) a -> Eff r a
runLift = interpret liftToM


runM :: Monad m => Eff '[Lift m] a -> m a
runM (Eff freer) = foldFreer (liftToM . extract) freer
