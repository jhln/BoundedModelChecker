{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module TreeSolver where

import Tree
import Solver
import FD
import Queue

solve :: Solver solver => Tree solver a -> [a]
solve = run . eval

eval :: Solver solver => Tree solver a  -> solver [a]
eval model = eval' model []

eval' :: Solver solver =>
         Tree solver a -> [(Label solver,Tree solver a)] -> solver [a]
eval' (Return x) wl = do
  xs <- continue wl
  return (x:xs)
eval' (Add c t) wl = do
  b <- add c
  if b then eval' t wl
    else continue wl
eval' (NewVar f) wl = do
  v <- newvar
  eval' (f v) wl
eval' (Try l r) wl = do
  now <- mark
  eval' l ((now, r):wl)
eval' Fail wl = continue wl
eval' (Dynamic m) wl = do
  t <- m
  eval' t wl


continue [] = return []
continue ((past,t):wl) = do
  goto past
  eval' t wl

evalq :: Solver solver => Tree solver a -> solver [a]
evalq model = evalq' model (emptyQ [])

evalq' :: (Solver solver, Queue q, Elem q ~ (Label solver, Tree solver a)) =>
          Tree solver a -> q -> solver [a]
evalq' (Return x) wl = do
  xs <- continueq wl
  return (x:xs)
evalq' (Add c t) wl = do
  b <- add c
  if b then evalq' t wl
    else continueq wl
evalq' (NewVar f) wl = do
  v <- newvar
  evalq' (f v) wl
evalq' (Try l r) wl = do
  now <- mark
  continueq (pushQ (now, l) (pushQ (now, r) wl))
evalq' Fail wl = continueq wl
evalq' (Dynamic m) wl = do
  t <- m
  evalq' t wl

continueq wl
  | isEmptyQ wl = return []
  | otherwise = let ((past, t), wl') = popQ wl
                    in do goto past
                          evalq' t wl'
