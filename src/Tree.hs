{-# LANGUAGE TypeFamilies #-}
module Tree where

import Control.Monad

import Solver


data Tree solver a
  = Return a
  | NewVar (Term solver -> Tree solver a)
  | Add (Constraint solver) (Tree solver a)
  | Try (Tree solver a) (Tree solver a)
  | Fail
  | Dynamic (solver (Tree solver a))


instance Functor (Tree solver) where
  --fmap f xs  =  xs >>= return . f
  fmap = liftM


instance Applicative (Tree solver) where
  pure = return
  (<*>) = ap

instance Monad (Tree solver) where
  return = Return
  (>>=) = extendTree

--extendTree :: Tree solver a -> (a -> Tree solver b) -> Tree solver b
(Return x) `extendTree` k = k x
(NewVar f) `extendTree` k = NewVar (\v -> f v `extendTree` k)
(Add c t) `extendTree` k = Add c (t `extendTree` k)
Fail `extendTree` k = Fail
(Try l r) `extendTree` k = Try (l `extendTree` k) (r `extendTree` k)
--(Dynamic m) `extendTree` k = Dynamic (do
--                                         t <- m
--                                         return t `extendTree` k)
