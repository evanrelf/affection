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

module Effect.Internal.OpenUnion where

import Data.Kind (Constraint, Type)


type Effect = (Type -> Type) -> Type -> Type


data Union (r :: [Effect]) m a where
  This :: e m a -> Union (e ': r) m a
  Next :: Union r m a -> Union (any ': r) m a


class Member e r where
  inject :: e m a -> Union r m a
  project :: Union r m a -> Maybe (e m a)


instance Member e (e ': r) where
  inject :: e m a -> Union (e ': r) m a
  inject = This

  project :: Union (e ': r) m a -> Maybe (e m a)
  project = \case
    This x -> Just x
    Next _ -> Nothing


instance {-# OVERLAPPABLE #-} Member e r => Member e (any ': r) where
  inject :: e m a -> Union (any ': r) m a
  inject = Next . inject

  project :: Union (any ': r) m a -> Maybe (e m a)
  project = \case
    Next u -> project u
    This _ -> Nothing


type family Members es r :: Constraint where
  Members '[] r = ()
  Members (e ': es) r = (Member e r, Members es r)


decompose :: Union (e ': r) m a -> Either (Union r m a) (e m a)
decompose = \case
  This x -> Right x
  Next u -> Left u


weaken :: Union r m a -> Union (any ': r) m a
weaken = Next


extract :: Union '[e] m a -> e m a
extract = \case
  This x -> x
  Next u -> case u of
