{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Reader
  ( Reader (..)
  , ask
  , runReader
  )
where

import Effect (Eff, Member, interpret, send)


data Reader r a where
  Ask :: Reader r r


ask :: Member (Reader i) r => Eff r i
ask = send Ask


readerToM :: Monad m => i -> Reader i a -> m a
readerToM i = \case
  Ask -> pure i


runReader
  :: forall m r i a
   . Monad m
  => Member m r
  => i
  -> Eff (Reader i ': r) a
  -> Eff r a
runReader i = interpret (readerToM @m i)
