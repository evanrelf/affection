{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Effect.Internal where

import Control.Monad.IO.Class (MonadIO (..))
import Effect.Internal.Eff (Eff (..), foldEff, hoistEff, liftEff)
import Effect.Internal.OpenUnion (Member (..), decompose, extract, inject)


send :: Member e r => e a -> Eff r a
send = liftEff . inject


interpret
  :: forall e2 e1 r a
   . Member e2 r
  => (forall x. e1 x -> e2 x)
  -> Eff (e1 ': r) a
  -> Eff r a
interpret handler = hoistEff $ \union ->
  case decompose union of
    Left u -> u
    Right e -> inject (handler e)


instance Member IO r => MonadIO (Eff r) where
  liftIO = send


runM :: Monad m => Eff '[m] a -> m a
runM eff = foldEff extract eff
