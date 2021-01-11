{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Internal.OpenUnion where

import Data.Kind (Constraint, Type)


type Effect = Type -> Type


data Union (r :: [Effect]) a where
  This :: e a -> Union (e ': r) a
  Next :: Union r a -> Union (any ': r) a


class Member e r where
  inject :: e a -> Union r a
  project :: Union r a -> Maybe (e a)


instance Member e (e ': r) where
  inject :: e a -> Union (e ': r) a
  inject = This

  project :: Union (e ': r) a -> Maybe (e a)
  project = \case
    This x -> Just x
    Next _ -> Nothing


instance {-# OVERLAPPABLE #-} Member e r => Member e (any ': r) where
  inject :: e a -> Union (any ': r) a
  inject = Next . inject

  project :: Union (any ': r) a -> Maybe (e a)
  project = \case
    Next u -> project u
    This _ -> Nothing


type family Members es r :: Constraint where
  Members '[] r = ()
  Members (e ': es) r = (Member e r, Members es r)


decompose :: Union (e ': r) a -> Either (Union r a) (e a)
decompose = \case
  This x -> Right x
  Next u -> Left u


weaken :: Union r a -> Union (any ': r) a
weaken = Next


extract :: Union '[e] a -> e a
extract = \case
  This x -> x
  Next u -> case u of
