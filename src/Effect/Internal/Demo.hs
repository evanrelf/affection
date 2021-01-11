{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Effect.Internal.Demo where

import Data.Function ((&))
import Effect (Eff, Members, runM)
import Effect.Bell (Bell, ringBell, runBellIO)
import Effect.Teletype (Teletype, readLine, runTeletypeIO, writeLine)


program :: Members '[Teletype, Bell] es => Eff es ()
program = do
  message <- readLine

  if message == "Ring the bell!" then do
    ringBell
    writeLine "Rang the bell!"

  else
    writeLine "Didn't ring the bell"


main :: IO ()
main = do
  program
    & runTeletypeIO
    & runBellIO
    & runM
