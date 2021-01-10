{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Union where

import Data.Kind (Type)


data Union (xs :: [Type -> Type]) a where
  Empty :: Union '[] a
  Zero :: f a -> Union (x ': xs) a
  Succ :: Union xs a -> Union (x ': xs) a


empty :: Union '[] Bool
empty = Empty


one :: Union '[Maybe] Bool
one = Zero $ Just True


two1 :: Union '[Maybe, Either ()] Bool
two1 = Zero $ Just True


two2 :: Union '[Maybe, Either ()] Bool
two2 = Succ $ Zero $ Right True
