{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Effect.Internal.Demo where

import Control.Monad.Free (Free (..), foldFree)
import Data.OpenUnion (Member (..), Union)
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
teletypeMain = foldFree teletypeToIO teletypeProgram


type TeletypeAndBell = Union '[Teletype, Bell]


send :: (Functor e, Member e es) => e (Free (Union es) a) -> Free (Union es) a
send = Free . inject


readLine' :: Member Teletype es => Free (Union es) String
readLine' = send $ ReadLine pure


writeLine' :: Member Teletype es => String -> Free (Union es) ()
writeLine' message = send $ WriteLine message (pure ())


ringBell' :: Member Bell es => Free (Union es) ()
ringBell' = send $ RingBell (pure ())


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
-- teletypeAndBellMain = foldFree teletypeAndBellToIO teletypeAndBellProgram
