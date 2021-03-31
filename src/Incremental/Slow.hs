module Incremental.Slow
  ( Rule
  , runRule
  ) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category
import Data.Functor.Identity

import Incremental.Arrow

newtype Rule m a b = Rule { runRule :: a -> m (Rule m a b, b) }

instance Monad m => Functor (Rule m a) where
  fmap f g = arr f . g
  {-# INLINE fmap #-}

instance Monad m => Category (Rule m) where
  {-# SPECIALIZE instance Category (Rule Identity) #-}
  id = arr id
  {-# INLINE id #-}
  Rule g . Rule f = Rule \a -> do
    (f', b) <- f a
    (g', c) <- g b
    pure (g' . f', c)
  {-# INLINABLE (.) #-}

instance Monad m => Arrow (Rule m) where
  {-# SPECIALIZE instance Arrow (Rule Identity) #-}
  arr f = go where
    go = Rule \a -> pure (go, f a)
  {-# INLINE arr #-}
  first (Rule f) = Rule \(a, c) -> do
    (f', b) <- f a
    pure (first f', (b, c))
  {-# INLINABLE first #-}

instance Monad m => ArrowChoice (Rule m) where
  {-# SPECIALIZE instance ArrowChoice (Rule Identity) #-}
  left f0 = go f0 where
    go (Rule f) = Rule \case
      Left a -> do
        (f', b) <- f a
        pure (go f', Left b)
      Right a ->
        pure (go f0, Right a)
  {-# INLINABLE left #-}
  f0 ||| g0 = go f0 g0 where
    go (Rule f) (Rule g) = Rule \case
      Left a -> do
        (f', b) <- f a
        pure (go f' g0, b)
      Right a -> do
        (g', b) <- g a
        pure (go f0 g', b)
  {-# INLINABLE (|||) #-}

instance Monad m => ArrowLazy (Rule m) where
  lazyA (Rule f) = Rule \a -> f a
  {-# INLINE lazyA #-}

instance Monad m => ArrowKleisli m (Rule m) where
  arrM f = go where
    go = Rule \a -> (go,) <$> f a
  {-# INLINABLE arrM #-}

instance Monad m => ArrowCache (Rule m) where
  cache f0 = go f0 Nothing where
    go (Rule f) s = Rule \a -> case s of
      Just (a', b) | a == a' -> pure (go (Rule f) s, b)
      _ -> do
        (f', b) <- f a
        pure (go f' (Just (a, b)), b)
  {-# INLINABLE cache #-}
