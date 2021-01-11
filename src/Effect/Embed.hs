{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Embed
  ( Embed
  , embed
  , runEmbed
  , runM
  )
where

import Effect (Member, interpret, send)
import Effect.Internal.Eff (Eff (..))
import Effect.Internal.Freer (foldFreer)
import Effect.Internal.OpenUnion (extract)


data Embed m a where
  Embed :: Monad m => m a -> Embed m a


embed :: (Monad m, Member (Embed m) es) => m a -> Eff es a
embed m = send $ Embed m


embedToM :: Embed m a -> m a
embedToM = \case
  Embed m -> m


runEmbed :: Member m es => Eff (Embed m ': es) a -> Eff es a
runEmbed = interpret embedToM


runM :: Monad m => Eff '[Embed m] a -> m a
runM (Eff freer) = foldFreer (embedToM . extract) freer
