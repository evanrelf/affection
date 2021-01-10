{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Effect.Teletype
  ( Teletype (..)
  , readLine
  , writeLine
  , runTeletypeIO
  )
where

import Control.Monad.Free (Free (..), runFree)


data Teletype k
  = ReadLine (String -> k)
  | WriteLine String k
  deriving Functor


readLine :: Free Teletype String
readLine = Free $ ReadLine pure


writeLine :: String -> Free Teletype ()
writeLine message = Free $ WriteLine message (pure ())


runTeletypeIO :: Free Teletype a -> IO a
runTeletypeIO = runFree \case
  ReadLine k -> do
    message <- getLine
    pure $ k message

  WriteLine message k -> do
    putStrLn message
    pure k
