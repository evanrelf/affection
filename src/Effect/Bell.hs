{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Effect.Bell
  ( Bell (..)
  , ringBell
  , bellToIO
  )
where

import Control.Monad.Free (Free (..))


data Bell k
  = RingBell k
  deriving Functor


ringBell :: Free Bell ()
ringBell = Free $ RingBell (pure ())


bellToIO :: Bell a -> IO a
bellToIO = \case
  RingBell k -> do
    putStrLn "DING"
    pure k
