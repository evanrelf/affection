{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Free
  ( Free (..)
  , liftFree
  , runFree
  , foldFree
  )
where


data Free f a
  = Pure a
  | Free (f (Free f a))


instance Functor f => Functor (Free f) where
  fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Pure x) = Pure (f x)
  fmap f (Free x) = Free (fmap (fmap f) x)


instance Functor f => Applicative (Free f) where
  pure :: a -> Free f a
  pure x = Pure x

  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  (<*>) (Pure f) x = fmap f x
  (<*>) (Free f) x = Free (fmap (<*> x) f)


instance Functor f => Monad (Free f) where
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (>>=) (Pure x) f = f x
  (>>=) (Free x) f = Free (fmap (>>= f) x)


liftFree :: Functor f => f a -> Free f a
liftFree x = Free (fmap Pure x)


runFree :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
runFree transform = \case
  Pure x -> pure x
  Free x -> transform x >>= runFree transform


foldFree :: Functor f => (forall x. f x -> x) -> Free f a -> a
foldFree fold = \case
  Pure x -> x
  Free x -> fold (fmap (foldFree fold) x)
