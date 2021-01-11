{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Effect.Internal.Free (foldFree)
import Effect.Internal.OpenUnion (extract)


data Embed m a
  = forall x. Embed (m x) (x -> a)


deriving instance Functor (Embed m)


embed :: Member (Embed m) es => m a -> Eff es a
embed m = send $ Embed m id


embedToM :: Monad m => Embed m a -> m a
embedToM = \case
  Embed m k -> do
    x <- m
    pure $ k x


runEmbed :: (Monad m, Member m es) => Eff (Embed m ': es) a -> Eff es a
runEmbed = interpret embedToM


runM :: Monad m => Eff '[Embed m] a -> m a
runM (Eff free) = foldFree (embedToM . extract) free
