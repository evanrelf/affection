{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Affection (Eff, Member, Members, interpret, runM, send)
import Affection.Reader (Reader, ask, runReader)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))


-- Bell


data Bell a where
  RingBell :: Bell ()


ringBell :: Member Bell r => Eff r ()
ringBell = send RingBell


runBellIO :: Member IO r => Eff (Bell : r) a -> Eff r a
runBellIO = interpret @IO $ \case
  RingBell -> putStrLn "DING"


-- Teletype


data Teletype a where
  ReadLine :: Teletype String
  WriteLine :: String -> Teletype ()


readLine :: Member Teletype r => Eff r String
readLine = send ReadLine


writeLine :: Member Teletype r => String -> Eff r ()
writeLine message = send $ WriteLine message


runTeletypeIO :: Member IO r => Eff (Teletype : r) a -> Eff r a
runTeletypeIO = interpret @IO $ \case
  ReadLine -> getLine
  WriteLine message -> putStrLn message


-- Program


program :: Members [Reader String, Teletype, Bell, IO] r => Eff r ()
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
