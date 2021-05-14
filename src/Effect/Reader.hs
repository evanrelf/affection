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
  , asks
  , runReader
  )
where

import Effect (Eff, Member, interpret, send)


data Reader i a where
  Ask :: Reader i i


ask :: Member (Reader i) r => Eff r i
ask = send Ask


asks :: Member (Reader i) r => (i -> j) -> Eff r j
asks f = fmap f ask


runReader
  :: forall m r i a
   . Monad m
  => Member m r
  => i
  -> Eff (Reader i ': r) a
  -> Eff r a
runReader i = interpret @m $ \case
  Ask -> pure i
