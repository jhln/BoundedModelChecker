{-# LANGUAGE FlexibleInstances #-}
module TreeSolver where



import Tree
import Solver
import FD

solve :: Solver solver => Tree solver a -> [a]
solve = run . eval

eval :: Solver solver => Tree solver a  -> solver [a]
eval model = eval' model []

eval' :: Solver solver => Tree solver a -> [(Label solver,Tree solver a)] -> solver [a]
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
