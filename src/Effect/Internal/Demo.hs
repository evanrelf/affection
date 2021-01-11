{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Effect.Internal.Demo where

import Data.Function ((&))
import Effect (Eff, Members, runM)
import Effect.Bell (Bell, ringBell, runBellIO)
import Effect.Reader (Reader, ask, runReader)
import Effect.Teletype (Teletype, readLine, runTeletypeIO, writeLine)


program :: Members '[Reader String, Teletype, Bell] es => Eff es ()
program = do
  message <- readLine

  triggerPhrase <- ask

  if message == triggerPhrase then do
    ringBell
    writeLine "Rang the bell!"

  else
    writeLine "Didn't ring the bell"


main :: IO ()
main = do
  program
    & runReader @IO "Ring the bell!"
    & runTeletypeIO
    & runBellIO
    & runM
