{-# LANGUAGE TypeFamilies #-}
module TreeSugar where

import Tree
import Solver
import FD


exist n k = f n []
  where
    f 0 acc = k acc
    f m acc = NewVar (\v -> f (m-1) (v:acc))

(@=) :: FDExpr -> Int -> Tree FD ()
e @= n = Add (Same e (Const n)) true

--(@/=) :: Term solver -> Term solver -> Tree solver a
e1 @/= e2 = Add (Diff e1 e2) true

(/\) :: Tree FD a -> Tree FD b -> Tree FD b
(/\) = (>>)

(\/) = Try

conj :: (Foldable t) => t (Tree FD ()) -> Tree FD ()
conj = foldl (/\) true

true :: (Solver solver) => Tree solver ()
true = Return ()
