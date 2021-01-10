{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Union
  ( Union (..)
  , Member (..)
  , decompose
  , weaken
  , extract
  )
where

import Data.Kind (Type)


data Union (es :: [Type -> Type]) a where
  This :: e a -> Union (e ': es) a
  Next :: Union es a -> Union (any ': es) a


class Member e es where
  inject :: e a -> Union es a
  project :: Union es a -> Maybe (e a)


instance Member e (e ': rest) where
  inject = This
  project = \case
    This x -> Just x
    Next _ -> Nothing


instance {-# OVERLAPPABLE #-} Member e es => Member e (any ': es) where
  inject = Next . inject
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
