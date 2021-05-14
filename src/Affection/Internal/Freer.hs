{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Affection.Internal.Freer where


newtype Freer f a = Freer
  { runFreer
      :: forall m
       . Monad m
      => (forall x. f x -> m x)
      -> m a
  }


instance Functor (Freer f) where
  fmap :: (a -> b) -> Freer f a -> Freer f b
  fmap a2b freer = Freer $ \fx2mx ->
    let
      x = runFreer freer fx2mx
    in
      fmap a2b x


instance Applicative (Freer f) where
  pure :: a -> Freer f a
  pure x = Freer $ \_ -> pure x

  (<*>) :: Freer f (a -> b) -> Freer f a -> Freer f b
  (<*>) freerF freerX = Freer $ \fx2mx ->
    let
      f = runFreer freerF fx2mx
      x = runFreer freerX fx2mx
    in
      f <*> x


instance Monad (Freer f) where
  (>>=) :: Freer f a -> (a -> Freer f b) -> Freer f b
  (>>=) freer a2Fb = Freer $ \fx2mx ->
    let
      m = runFreer freer fx2mx
      k x = runFreer (a2Fb x) fx2mx
    in
      m >>= k


liftFreer :: f a -> Freer f a
liftFreer fa = Freer $ \fx2mx -> fx2mx fa


hoistFreer :: (forall x. f x -> g x) -> Freer f a -> Freer g a
hoistFreer fx2gx freer = Freer $ \gx2mx -> runFreer freer (gx2mx . fx2gx)


foldFreer :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
foldFreer fx2mx freer = runFreer freer fx2mx
