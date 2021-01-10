{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Effect.Teletype
  ( Teletype (..)
  , readLine
  , writeLine
  , teletypeToIO
  )
where

import Control.Monad.Free (Free (..))


data Teletype k
  = ReadLine (String -> k)
  | WriteLine String k
  deriving Functor


readLine :: Free Teletype String
readLine = Free $ ReadLine pure


writeLine :: String -> Free Teletype ()
writeLine message = Free $ WriteLine message (pure ())


teletypeToIO :: Teletype a -> IO a
teletypeToIO = \case
  ReadLine k -> do
    message <- getLine
    pure $ k message

  WriteLine message k -> do
    putStrLn message
    pure k
