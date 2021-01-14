{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Effect.Lift
  ( Lift
  , lift
  , runM
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Effect (Member, send)
import Effect.Internal.Eff (Eff (..), foldEff)
import Effect.Internal.OpenUnion (extract)


data Lift m z a where
  Lift :: Monad m => { unLift :: m a } -> Lift m z a


lift :: (Monad m, Member (Lift m) r) => m a -> Eff r a
lift m = send $ Lift m


runM :: Monad m => Eff '[Lift m] a -> m a
runM eff = foldEff (unLift . extract) eff


instance Member (Lift IO) r => MonadIO (Eff r) where
  liftIO = lift
