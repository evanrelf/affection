{-# LANGUAGE DataKinds #-}

module Effect.Internal.Demo where

-- import Data.Function ((&))
-- import Effect (Eff, Members, runM)
-- import Effect.Bell (Bell, ringBell, runBellIO)
-- import Effect.Lift (Lift, lift, runLift)
-- import Effect.Reader (Reader, ask, runReader)
-- import Effect.Teletype (Teletype, readLine, runTeletypeIO, writeLine)


-- program :: Members '[Reader String, Teletype, Bell, Lift IO] r => Eff r ()
-- program = do
--   message <- readLine

--   triggerPhrase <- ask

--   if message == triggerPhrase then do
--     ringBell
--     writeLine "Rang the bell!"

--   else
--     writeLine "Didn't ring the bell"

--   lift $ putStrLn "All done"


-- main :: IO ()
-- main = do
--   program
--     & runReader @IO "Ring the bell!"
--     & runTeletypeIO
--     & runBellIO
--     & runLift @IO
--     & runM
