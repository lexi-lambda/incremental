{-# OPTIONS_GHC -dno-typeable-binds -dsuppress-all #-}

module Example.Slow where

import Control.Arrow
import Data.Functor.Identity

import Incremental.Slow

traverseMaybe :: Rule Identity a b -> Rule Identity (Maybe a) (Maybe b)
traverseMaybe f = proc v -> case v of
  Just a  -> Just <$> f -< a
  Nothing -> returnA -< Nothing
