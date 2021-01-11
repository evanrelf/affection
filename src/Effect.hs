{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Monad.Free (Free, foldFree, hoistFree, liftFree)
import Data.OpenUnion (Member (..), Members, Union, decompose, extract)


newtype Eff es a = Eff (Free (Union es) a)
  deriving newtype (Functor, Applicative, Monad)


send :: (Functor e, Member e es) => e a -> Eff es a
send = Eff . liftFree . inject


interpret
  :: forall e1 e2 es a
   . Functor e2
  => Member e2 es
  => (forall x. e1 x -> e2 x)
  -> Eff (e1 ': es) a
  -> Eff es a
interpret handler (Eff free) = Eff (hoistFree f free)
  where
  f :: forall x. Union (e1 ': es) x -> Union es x
  f union =
    case decompose union of
      Left union' -> union'
      Right e1 -> inject (handler e1)


runM :: Monad m => Eff '[m] a -> m a
runM (Eff free) = foldFree extract free
