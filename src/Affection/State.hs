{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Affection.State
  ( State (..)
  , get
  , gets
  , put
  , modify
  , runState
  , evalState
  , execState
  )
where

import Affection (Eff, Member, stateful, send)


data State i a where
  Get :: State s s
  Put :: s -> State s ()
  Modify :: (s -> s) -> State s ()


get :: Member (State s) r => Eff r s
get = send Get


gets :: Member (State s) r => (s -> s') -> Eff r s'
gets f = fmap f get


put :: Member (State s) r => s -> Eff r ()
put s = send (Put s)


modify :: Member (State s) r => (s -> s) -> Eff r ()
modify f = send (Modify f)


runState :: forall r s a. s -> Eff (State s : r) a -> Eff r (s, a)
runState = stateful $ \case
  Get -> \s -> pure (s, s)
  Put s' -> \_ -> pure (s', ())
  Modify f -> \s -> pure (f s, ())


evalState :: forall r s a. s -> Eff (State s : r) a -> Eff r a
evalState s eff = snd <$> runState s eff


execState :: forall r s a. s -> Eff (State s : r) a -> Eff r s
execState s eff = fst <$> runState s eff
