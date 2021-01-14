{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Bell
  ( Bell (..)
  , ringBell
  , runBellIO
  )
where

import Effect (Eff, Member, interpret, send)
import Effect.Lift (Lift, lift)


data Bell m a where
  RingBell :: Bell m ()


ringBell :: Member Bell r => Eff r ()
ringBell = send RingBell


runBellIO :: Member (Lift IO) r => Eff (Bell ': r) a -> Eff r a
runBellIO = interpret \case
  RingBell -> lift $ putStrLn "DING"
