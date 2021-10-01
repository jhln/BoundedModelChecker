{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module ComposableTransformers where

import Solver
import Tree
import Transformers

class CTransformer c where
  type CEvalState c :: *
  type CTreeState c :: *
  leftCT, rightCT :: c -> CTreeState c -> CTreeState c
  leftCT _  = id
  rightCT = leftCT
  nextCT :: CSearchSig solver c a
  nextCT = evalct'
  initCT :: c -> (CEvalState c, CTreeState c)


type CSearchSig solver c a =
  (Solver solver, CTransformer c) =>
     Tree solver a -> c ->
        CEvalState c -> CTreeState c ->
           EVAL solver c a -> CONTINUE solver c a -> solver [a]

type EVAL solver c a = (Tree solver a -> CEvalState c -> solver [a])

type CONTINUE solver c a = (CEvalState c -> solver [a])

evalct' :: CSearchSig solver c a
evalct' = undefined



data Composition es ts where
  (:-) :: (CTransformer a, CTransformer b) => a -> b ->
          Composition (CEvalState a, CEvalState b)
                      (CTreeState a, CTreeState b)


instance CTransformer (Composition es ts) where
  type CEvalState (Composition es ts) = es
  type CTreeState (Composition es ts) = ts
  initCT (c1 :- c2) = let (es1, ts1) = initCT c1
                          (es2, ts2) = initCT c2
                          in ((es1, es2), (ts1, ts2))
  leftCT (c1 :- c2) (ts1, ts2) = (leftCT c1 ts1, leftCT c2 ts2)
  rightCT (c1 :- c2) (ts1,ts2) = (rightCT c1 ts1, rightCT c2 ts2)
  nextCT tree (c1 :- c2) (es1, es2) (ts1, ts2) eval continue =
    nextCT tree c1 es1 ts1 (\tree' es1' ->
                              nextCT tree' c2 es2 ts2 (\tree'' es2' ->
                                                         eval tree'' (es1', es2'))
                              (\es2' -> continue (es1', es2')))
    (\es1' -> continue (es1', es2))


data TStack es ts where
  TStack :: CTransformer c => c -> TStack (CEvalState c) (CTreeState c)



instance Transformer (TStack es ts) where
  type EvalState (TStack es ts) = es
  type TreeState (TStack es ts) = ts
  initT (TStack c) = initCT c
  leftT (TStack c) = leftCT c
  rightT (TStack c) = rightCT c
  nextT tree q t@(TStack c) es ts =
    nextCT tree c es ts
    (\tree' es' -> evalt' tree' q t es' ts)
    (continuet q t)
