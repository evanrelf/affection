{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Bell
  ( Bell (..)
  , ringBell
  , runBellIO
  )
where

import Effect (Eff, Member, interpret, send)


data Bell a where
  RingBell :: Bell ()


ringBell :: Member Bell r => Eff r ()
ringBell = send RingBell


bellToIO :: Bell a -> IO a
bellToIO = \case
  RingBell -> putStrLn "DING"


runBellIO :: Member IO r => Eff (Bell ': r) a -> Eff r a
runBellIO = interpret bellToIO
