{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstrainedClassMethods #-}
module Transformers where

import Data.Sequence

import System.Random

import Solver
import Tree
import Queue

class Transformer t where
  type EvalState t :: *
  type TreeState t :: *
  leftT, rightT :: t -> TreeState t -> TreeState t
  leftT _  = id
  rightT = leftT
  nextT :: SearchSig solver q t a
  nextT = evalt'
  initT :: t -> (EvalState t, TreeState t)
--  tryT :: SearchSig solver q t a
--  tryT = evalt'

type SearchSig solver q t a =
  (Solver solver, Queue q,
    Transformer t, Elem q ~(Label solver, Tree solver a, TreeState t)) =>
     Tree solver a -> q -> t -> EvalState t -> TreeState t -> solver [a]


evalt :: (Transformer t) => Solver solver => Tree solver a -> t -> solver [a]
evalt model transformer = evalt' model empty transformer es ts
  where
    (es, ts) = initT transformer

--evalt' :: (Solver solver, Queue q, Elem q ~ (Label solver, Tree solver a)) =>
--          Tree solver a -> q -> solver [a]
evalt' :: SearchSig solver q t a
evalt' (Return x) wl t es _ = do
  xs <- continuet wl t es
  return (x:xs)
evalt' (Add c tr) wl t es ts = do
  b <- add c
  if b then evalt' tr wl t es ts
    else continuet wl t es
evalt' (NewVar f) wl t es ts = do
  v <- newvar
  evalt' (f v) wl t es ts
evalt' (Try l r) wl t es ts = do
  now <- mark
  let wl' = pushQ (now, l, leftT t ts) (pushQ (now, r, rightT t ts) wl)
  continuet wl' t es
evalt' Fail wl t es _ = continuet wl t es
evalt' (Dynamic m) wl t es ts = do
  tr <- m
  evalt' tr wl t es ts


continuet wl t es
  | isEmptyQ wl = return []
  | otherwise = let ((past,tree,ts),wl') = popQ wl
    in do goto past
          nextT tree wl' t es ts

newtype DepthBoundedST = DBST Int

instance Transformer DepthBoundedST where
  type EvalState DepthBoundedST = Bool
  type TreeState DepthBoundedST = Int
  initT (DBST n) = (False, n)
  leftT _ ts = ts - 1
  nextT tree q t es ts
    | ts == 0 = continuet q t True
    | otherwise = evalt' tree q t es ts

newtype RandomizeST = RDST Int

instance Transformer RandomizeST where
  type EvalState RandomizeST = [Bool]
  type TreeState RandomizeST = ()
  initT (RDST seed) = (randoms (mkStdGen seed),())

-- TODO: what is this?
tryT (Try l r) q t (b:bs) ts =
    if b then evalt' (Try r l) q t bs ts
    else evalt' (Try l r) q t bs ts
