module Effect.Internal.Demo where

import Control.Monad.Free (Free (..), runFree)
import Data.Functor.Sum (Sum (..))
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


type TeletypeAndBell = Sum Teletype Bell


readLine' :: Free TeletypeAndBell String
readLine' = Free $ InL $ ReadLine pure


writeLine' :: String -> Free TeletypeAndBell ()
writeLine' message = Free $ InL $ WriteLine message (pure ())


ringBell' :: Free TeletypeAndBell ()
ringBell' = Free $ InR $ RingBell (pure ())


teletypeAndBellToIO :: TeletypeAndBell a -> IO a
teletypeAndBellToIO = sumToM teletypeToIO bellToIO


teletypeAndBellProgram :: Free TeletypeAndBell ()
teletypeAndBellProgram = do
  message <- readLine'

  if message == "Ring the bell!" then do
    ringBell'
    writeLine' "Rang the bell!"

  else
    writeLine' "Didn't ring the bell"


teletypeAndBellMain :: IO ()
teletypeAndBellMain = runFree teletypeAndBellToIO teletypeAndBellProgram
