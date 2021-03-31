module Incremental.Arrow
  ( dup

  , ArrowLazy(..)
  , foldlA'
  , traverseA_
  , traverseA
  , onNothingA

  , ArrowKleisli(..)
  , bindA

  , ArrowCache(..)
  ) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category
import Data.Foldable

dup :: Arrow arr => arr a (a, a)
dup = arr \x -> (x, x)
{-# INLINE dup #-}

class Arrow arr => ArrowLazy arr where
  lazyA :: arr a b -> arr a b

instance ArrowLazy (->) where
  lazyA = id
  {-# INLINE lazyA #-}

foldlA' :: (ArrowChoice arr, ArrowLazy arr, Foldable t) => arr (e, b, a) b -> arr (e, b, t a) b
foldlA' f = arr (\(e, v, xs) -> (e, v, toList xs)) >>> go where
  go = uncons >>> (id ||| step)
  uncons = arr \(e, v, xs) -> case xs of
    []    -> Left v
    x:xs' -> Right ((e, v, x), (e, xs'))
  step = first f >>> arr (\(!v, (e, xs)) -> (e, v, xs)) >>> lazyA go
{-# INLINABLE foldlA' #-}

traverseA_ :: (ArrowChoice arr, ArrowLazy arr, Foldable t) => arr (e, a) b -> arr (e, t a) ()
traverseA_ f = proc (e, xs) ->
  (| foldlA' (\() x -> do { (e, x) >- f; () >- returnA }) |) () xs
{-# INLINABLE traverseA_ #-}

data Traversal a r b
  = Done b
  | Yield a !(r -> Traversal a r b)

instance Functor (Traversal a r) where
  fmap f = \case
    Done x -> Done (f x)
    Yield v k -> Yield v (fmap f . k)

instance Applicative (Traversal a r) where
  pure = Done
  tf <*> tx = case tf of
    Done f    -> fmap f tx
    Yield v k -> Yield v ((<*> tx) . k)

traversal :: Traversable t => t a -> Traversal a b (t b)
traversal = traverse (flip Yield Done)

traverseA :: (ArrowChoice arr, ArrowLazy arr, Traversable t) => arr (e, a) b -> arr (e, t a) (t b)
traverseA f = second (arr traversal) >>> go where
  go = proc (e, as) -> case as of
    Done bs -> returnA -< bs
    Yield a k -> do
      b <- f -< (e, a)
      lazyA go -< (e, k b)
{-# INLINABLE [1] traverseA #-}

traverseA_Maybe :: ArrowChoice arr => arr (e, a) b -> arr (e, Maybe a) (Maybe b)
traverseA_Maybe f = proc (e, v) -> case v of
  Just a  -> arr Just . f -< (e, a)
  Nothing -> returnA -< Nothing
{-# INLINE traverseA_Maybe #-}
{-# RULES "traverseA @Maybe" traverseA = traverseA_Maybe #-}

traverseA_List :: (ArrowChoice arr, ArrowLazy arr) => arr (e, a) b -> arr (e, [a]) [b]
traverseA_List f = go where
  go = proc (e, v) -> case v of
    []   -> returnA -< []
    x:xs -> do
      y <- f -< (e, x)
      ys <- lazyA go -< (e, xs)
      returnA -< y:ys
{-# INLINABLE traverseA_List #-}
{-# RULES "traverseA @[]" traverseA = traverseA_List #-}

onNothingA :: ArrowChoice arr => arr e a -> arr (e, Maybe a) a
onNothingA f = proc (e, v) -> case v of
  Just a  -> returnA -< a
  Nothing -> f -< e
{-# INLINE onNothingA #-}

class (Arrow p, Monad m) => ArrowKleisli m p | p -> m where
  arrM :: (a -> m b) -> p a b

bindA :: ArrowKleisli m arr => arr (m a) a
bindA = arrM id
{-# INLINE bindA #-}

class Arrow arr => ArrowCache arr where
  cache :: Eq a => arr a b -> arr a b
