{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Affection (Eff, Members, runM)
import Affection.Bell (Bell, ringBell, runBellIO)
import Affection.Reader (Reader, ask, runReader)
import Affection.Teletype (Teletype, readLine, runTeletypeIO, writeLine)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))


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
