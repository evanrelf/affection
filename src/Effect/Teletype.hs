{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
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


data Teletype a
  = ReadLine (String -> a)
  | WriteLine String a
  deriving Functor


readLine :: Member Teletype es => Eff es String
readLine = send $ ReadLine id


writeLine :: Member Teletype es => String -> Eff es ()
writeLine message = send $ WriteLine message ()


teletypeToIO :: Teletype a -> IO a
teletypeToIO = \case
  ReadLine k -> do
    message <- getLine
    pure $ k message

  WriteLine message k -> do
    putStrLn message
    pure k


runTeletypeIO :: Member IO es => Eff (Teletype ': es) a -> Eff es a
runTeletypeIO = interpret teletypeToIO
