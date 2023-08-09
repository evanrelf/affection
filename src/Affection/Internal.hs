{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Affection.Internal where

import Affection.Internal.Eff (Eff (..), foldEff, liftEff)
import Affection.Internal.Union (Member (..), decompose, inject)


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
