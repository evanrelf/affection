{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Teletype
  ( Teletype (..)
  , readLine
  , writeLine
  , runTeletypeIO
  )
where

import Effect (Eff, Member, interpret, send)


data Teletype a where
  ReadLine :: Teletype String
  WriteLine :: String -> Teletype ()


readLine :: Member Teletype es => Eff es String
readLine = send ReadLine


writeLine :: Member Teletype es => String -> Eff es ()
writeLine message = send $ WriteLine message


teletypeToIO :: Teletype a -> IO a
teletypeToIO = \case
  ReadLine -> getLine
  WriteLine message -> putStrLn message


runTeletypeIO :: Member IO es => Eff (Teletype ': es) a -> Eff es a
runTeletypeIO = interpret teletypeToIO
