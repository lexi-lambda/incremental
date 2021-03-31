module Incremental.Fast
  ( Rule
  , runRule
  ) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category
import Data.Functor
import Data.Functor.Identity

import Incremental.Arrow

data Result s a = Result !s a

data Rule m a b where
  Rule :: !s -> !(s -> a -> m (Result s b)) -> Rule m a b

mkRule :: Functor m => s -> (s -> a -> m (Result s b)) -> Rule m a b
mkRule = Rule
{-# INLINE CONLIKE [1] mkRule #-}
{-# RULES
"mkRule @((), _)" forall s f. mkRule ((), s) f =
  Rule s (\s1 a -> f ((), s1) a <&> \(Result ((), s2) b) -> Result s2 b)
"mkRule @(_, ())" forall s f. mkRule (s, ()) f =
  Rule s (\s1 a -> f (s1, ()) a <&> \(Result (s2, ()) b) -> Result s2 b)
#-}

runRule :: Functor m => Rule m a b -> a -> m (Rule m a b, b)
runRule (Rule s1 f) a = f s1 a <&> \(Result s2 b) -> (Rule s2 f, b)
{-# INLINE runRule #-}

instance Monad m => Functor (Rule m a) where
  fmap f g = arr f . g
  {-# INLINE fmap #-}

instance Monad m => Category (Rule m) where
  {-# SPECIALIZE instance Category (Rule Identity) #-}
  id = arr id
  {-# INLINE id #-}
  Rule t0 g . Rule s0 f = mkRule (s0, t0) \(s1, t1) a -> do
    Result s2 b <- f s1 a
    Result t2 c <- g t1 b
    pure $! Result (s2, t2) c
  {-# INLINE CONLIKE (.) #-}

instance Monad m => Arrow (Rule m) where
  arr f = Rule () \_ a -> pure $! Result () (f a)
  {-# INLINE arr #-}
  first (Rule s0 f) = Rule s0 \s1 (a, c) -> do
    Result s2 b <- f s1 a
    pure $! Result s2 (b, c)
  {-# INLINE first #-}
  second (Rule s0 f) = Rule s0 \s1 (c, a) -> do
    Result s2 b <- f s1 a
    pure $! Result s2 (c, b)
  {-# INLINE second #-}
  Rule s0 f *** Rule t0 g = mkRule (s0, t0) \(s1, t1) (a, b) -> do
    Result s2 c <- f s1 a
    Result t2 d <- g t1 b
    pure $! Result (s2, t2) (c, d)
  {-# INLINE (***) #-}
  Rule s0 f &&& Rule t0 g = mkRule (s0, t0) \(s1, t1) a -> do
    Result s2 b <- f s1 a
    Result t2 c <- g t1 a
    pure $! Result (s2, t2) (b, c)
  {-# INLINE (&&&) #-}

instance Monad m => ArrowChoice (Rule m) where
  left (Rule s0 f) = Rule s0 \s1 -> \case
    Left a -> do
      Result s2 b <- f s1 a
      pure $! Result s2 (Left b)
    Right a ->
      pure $! Result s0 (Right a)
  {-# INLINE left #-}
  right (Rule s0 f) = Rule s0 \s1 -> \case
    Left a ->
      pure $! Result s0 (Left a)
    Right a -> do
      Result s2 b <- f s1 a
      pure $! Result s2 (Right b)
  {-# INLINE right #-}
  f +++ g = (Left <$> f) ||| (Right <$> g)
  {-# INLINE (+++) #-}
  Rule s0 f ||| Rule t0 g = mkRule (s0, t0) \(s1, t1) -> \case
    Left a -> do
      Result s2 b <- f s1 a
      pure $! Result (s2, t0) b
    Right a -> do
      Result t2 b <- g t1 a
      pure $! Result (s0, t2) b
  {-# INLINE (|||) #-}

data Lazy a = Delay a

instance Monad m => ArrowLazy (Rule m) where
  lazyA r0 = Rule (Delay r0) \(Delay r1) a -> do
    (r2, b) <- runRule r1 a
    pure $! Result (Delay r2) b
  {-# INLINE lazyA #-}

instance Monad m => ArrowKleisli m (Rule m) where
  arrM f = Rule () \_ a -> Result () <$> f a
  {-# INLINE arrM #-}

instance Monad m => ArrowCache (Rule m) where
  cache (Rule s0 f) = Rule (s0, Nothing) \(s1, t1) a2 -> case t1 of
    Just (a1, b1) | a1 == a2 ->
      pure $! Result (s1, t1) b1
    _ -> do
      Result s2 b2 <- f s1 a2
      pure $! Result (s2, Just (a2, b2)) b2
  {-# INLINE cache #-}
