{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Affection.Internal.Demo where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Affection (Eff, Members, runM)
import Affection.Bell (Bell, ringBell, runBellIO)
import Affection.Reader (Reader, ask, runReader)
import Affection.Teletype (Teletype, readLine, runTeletypeIO, writeLine)


program :: Members '[Reader String, Teletype, Bell, IO] r => Eff r ()
program = do
  message <- readLine

  triggerPhrase <- ask

  if message == triggerPhrase then do
    ringBell
    writeLine "Rang the bell!"

  else
    writeLine "Didn't ring the bell"

  liftIO $ putStrLn "All done"


main :: IO ()
main = do
  program
    & runReader @IO "Ring the bell!"
    & runTeletypeIO
    & runBellIO
    & runM
