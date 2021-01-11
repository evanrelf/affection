{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Effect
  ( Eff
  , Member
  , Members
  , send
  , interpret
  , runM
  )
where

import Control.Monad.Free (Free, foldFree, liftFree)
import Data.OpenUnion (Member (..), Members, Union, extract)


newtype Eff es a = Eff (Free (Union es) a)
  deriving newtype (Functor, Applicative, Monad)


send :: (Functor e, Member e es) => e a -> Eff es a
send = Eff . liftFree . inject


interpret
  :: Member m es
  => (forall x. e x -> m x)
  -> Eff (e ': es) a
  -> Eff es a
interpret handler effect = undefined


runM :: Monad m => Eff '[m] a -> m a
runM (Eff m) = foldFree extract m
