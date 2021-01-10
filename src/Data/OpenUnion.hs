{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.OpenUnion
  ( Effect
  , Union
  , Member (..)
  , Members
  , decompose
  , weaken
  , extract
  )
where

import Data.Kind (Constraint, Type)


type Effect = Type -> Type


data Union (es :: [Effect]) a where
  This :: Functor e => e a -> Union (e ': es) a
  Next :: Union es a -> Union (any ': es) a


instance Functor (Union es) where
  fmap :: (a -> b) -> Union es a -> Union es b
  fmap f = \case
    This x -> This (fmap f x)
    Next u -> Next (fmap f u)


class Member e es where
  inject :: Functor e => e a -> Union es a
  project :: Union es a -> Maybe (e a)


instance Member e (e ': es) where
  inject :: Functor e => e a -> Union (e ': es) a
  inject = This

  project :: Union (e ': es) a -> Maybe (e a)
  project = \case
    This x -> Just x
    Next _ -> Nothing


type family Members (es :: [Effect]) (es' :: [Effect]) :: Constraint where
  Members '[] _ = ()
  Members (e ': es) es' = (Member e es', Members es es')


instance {-# OVERLAPPABLE #-} Member e es => Member e (any ': es) where
  inject :: Functor e => e a -> Union (any ': es) a
  inject = Next . inject

  project :: Union (any ': es) a -> Maybe (e a)
  project = \case
    Next u -> project u
    This _ -> Nothing


decompose :: Union (e ': es) a -> Either (Union es a) (e a)
decompose = \case
  This x -> Right x
  Next u -> Left u


weaken :: Union es a -> Union (any ': es) a
weaken = Next


extract :: Union '[e] a -> e a
extract = \case
  This x -> x
  Next u -> case u of
