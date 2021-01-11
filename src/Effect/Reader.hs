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


ask :: Member (Reader r) es => Eff es r
ask = send Ask


readerToM :: Monad m => r -> Reader r a -> m a
readerToM r = \case
  Ask -> pure r


runReader
  :: forall m r es a
   . Monad m
  => Member m es
  => r
  -> Eff (Reader r ': es) a
  -> Eff es a
runReader r = interpret (readerToM @m r)
