{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Affection.Bell
  ( Bell (..)
  , ringBell
  , runBellIO
  )
where

import Affection (Eff, Member, interpret, send)


data Bell a where
  RingBell :: Bell ()


ringBell :: Member Bell r => Eff r ()
ringBell = send RingBell


runBellIO :: Member IO r => Eff (Bell ': r) a -> Eff r a
runBellIO = interpret @IO $ \case
  RingBell -> putStrLn "DING"
