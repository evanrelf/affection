{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Effect.Internal.Freer where


newtype Freer f a
  = Freer { runFreer :: forall m. Monad m => (forall x. f x -> m x) -> m a }


instance Functor (Freer f) where
  fmap :: (a -> b) -> Freer f a -> Freer f b
  fmap a2b freer = Freer \fx2mx -> fmap a2b (runFreer freer fx2mx)


instance Applicative (Freer f) where
  pure :: a -> Freer f a
  pure x = Freer \_ -> pure x

  (<*>) :: Freer f (a -> b) -> Freer f a -> Freer f b
  (<*>) freerF freerX = Freer \fx2mx ->
    runFreer freerF fx2mx <*> runFreer freerX fx2mx


instance Monad (Freer f) where
  (>>=) :: Freer f a -> (a -> Freer f b) -> Freer f b
  (>>=) freer a2Fb = Freer \fx2mx -> do
    a <- runFreer freer fx2mx
    runFreer (a2Fb a) fx2mx


liftFreer :: f a -> Freer f a
liftFreer fa = Freer \fx2mx -> fx2mx fa


hoistFreer :: (forall x. f x -> g x) -> Freer f a -> Freer g a
hoistFreer fx2gx freer = Freer \gx2mx -> runFreer freer (gx2mx . fx2gx)


foldFreer :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
foldFreer fx2mx freer = runFreer freer fx2mx
