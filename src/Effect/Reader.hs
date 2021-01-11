{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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


data Reader r a
  = Ask (r -> a)


ask :: Member (Reader r) es => Eff es r
ask = send $ Ask id


readerToM :: Monad m => r -> Reader r a -> m a
readerToM r = \case
  Ask k ->
    pure $ k r


runReader
  :: forall m r es a
   . Monad m
  => Member m es
  => r
  -> Eff (Reader r ': es) a
  -> Eff es a
runReader r = interpret (readerToM @m r)
