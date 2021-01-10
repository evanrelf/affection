{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Effect.Internal.Demo where

import Control.Monad.Free (Free (..), runFree)
import Data.Union (Union, Member (..))
import Effect (sumToM)
import Effect.Bell (Bell (..), bellToIO)
import Effect.Teletype (Teletype (..), readLine, teletypeToIO, writeLine)


teletypeProgram :: Free Teletype ()
teletypeProgram = do
  message <- readLine

  if message == "Ring the bell!" then do
    writeLine "Rang the bell!"

  else
    writeLine "Didn't ring the bell"


teletypeMain :: IO ()
teletypeMain = runFree teletypeToIO teletypeProgram


type TeletypeAndBell = Union '[Teletype, Bell]


readLine' :: Member Teletype es => Free (Union es) String
readLine' = Free $ inject $ ReadLine pure


writeLine' :: Member Teletype es => String -> Free (Union es) ()
writeLine' message = Free $ inject $ WriteLine message (pure ())


ringBell' :: Member Bell es => Free (Union es) ()
ringBell' = Free $ inject $ RingBell (pure ())


-- teletypeAndBellToIO :: TeletypeAndBell a -> IO a
-- teletypeAndBellToIO = sumToM teletypeToIO bellToIO


-- teletypeAndBellProgram :: Free TeletypeAndBell ()
-- teletypeAndBellProgram = do
--   message <- readLine'

--   if message == "Ring the bell!" then do
--     ringBell'
--     writeLine' "Rang the bell!"

--   else
--     writeLine' "Didn't ring the bell"


-- teletypeAndBellMain :: IO ()
-- teletypeAndBellMain = runFree teletypeAndBellToIO teletypeAndBellProgram
