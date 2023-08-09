{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Affection.Internal where

import Control.Monad.IO.Class (MonadIO (..))
import Affection.Internal.Eff (Eff (..), foldEff, liftEff)
import Affection.Internal.Union (Member (..), decompose, extract, inject)


send :: Member e r => e a -> Eff r a
send = liftEff . inject


interpret
  :: forall e r a
   . (forall x. e x -> Eff r x)
  -> Eff (e : r) a
  -> Eff r a
interpret handler = foldEff $ \union ->
  case decompose union of
    Left u -> liftEff u
    Right e -> handler e


instance Member IO r => MonadIO (Eff r) where
  liftIO = send


runM :: Monad m => Eff '[m] a -> m a
runM eff = foldEff extract eff
