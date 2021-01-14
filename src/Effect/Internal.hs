{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Internal where

import Effect.Internal.Eff (Eff (..), hoistEff, liftEff)
import Effect.Internal.OpenUnion (Member (..), Union, decompose)


send :: Member e r => e (Eff r) a -> Eff r a
send = liftEff . inject


interpret
  :: forall e r a
   . (forall r0 x. e (Eff r0) x -> Eff r x)
  -> Eff (e ': r) a
  -> Eff r a
interpret handler eff = undefined
  where
  pop
    :: (forall x. e (Eff r) x -> Eff r x)
    -> Union (e ': r) (Eff r) a
    -> Eff r a
  pop handler union =
    case decompose union of
      Left union' -> liftEff union'
      Right e -> handler e
