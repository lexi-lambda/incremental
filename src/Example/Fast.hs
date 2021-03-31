{-# OPTIONS_GHC -dno-typeable-binds -dsuppress-all #-}

module Example.Fast where

import Control.Arrow
import Data.Functor.Identity

import Incremental.Fast

traverseMaybe :: Rule Identity a b -> Rule Identity (Maybe a) (Maybe b)
traverseMaybe f = proc v -> case v of
  Just a  -> Just <$> f -< a
  Nothing -> returnA -< Nothing
