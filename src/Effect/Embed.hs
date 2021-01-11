{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Embed
  ( Embed
  , embed
  , runEmbed
  , runEmbed'
  )
where

import Control.Monad.Free (foldFree)
import Data.OpenUnion (extract)
import Effect (Member, interpret, send)
import Effect.Internal (Eff (..))


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


runEmbed' :: Monad m => Eff '[Embed m] a -> m a
runEmbed' (Eff free) = foldFree (embedToM . extract) free
