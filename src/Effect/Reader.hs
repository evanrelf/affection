{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Reader
  ( Reader (..)
  , ask
  , asks
  , runReader
  )
where

import Effect (Eff, Member, interpret, send)


data Reader i m a where
  Ask :: Reader i m i


ask :: Member (Reader i) r => Eff r i
ask = send Ask


asks :: Member (Reader i) r => (i -> j) -> Eff r j
asks f = fmap f ask


runReader :: i -> Eff (Reader i ': r) a -> Eff r a
runReader i = interpret $ \case
  Ask -> pure i
