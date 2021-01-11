{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Bell
  ( Bell (..)
  , ringBell
  , runBellIO
  )
where

import Effect (Eff, Member, interpret, send)


data Bell a
  = RingBell a


ringBell :: Member Bell es => Eff es ()
ringBell = send $ RingBell ()


bellToIO :: Bell a -> IO a
bellToIO = \case
  RingBell k -> do
    putStrLn "DING"
    pure k


runBellIO :: Member IO es => Eff (Bell ': es) a -> Eff es a
runBellIO = interpret bellToIO
