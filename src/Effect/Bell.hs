{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Effect.Bell
  ( Bell (..)
  , ringBell
  , runBellIO
  )
where

import Control.Monad.Free (Free (..), runFree)


data Bell k
  = RingBell k
  deriving Functor


ringBell :: Free Bell ()
ringBell = Free $ RingBell (pure ())


runBellIO :: Free Bell a -> IO a
runBellIO = runFree \case
  RingBell k -> do
    putStrLn "DING"
    pure k
