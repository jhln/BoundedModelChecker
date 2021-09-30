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


in_domain :: FDExpr -> (Int, Int) -> Tree FD ()
in_domain v (l, u) = Add (Dom v l u ) true

--(@/=) :: Term solver -> Term solver -> Tree solver a
e1 @/= e2 = Add (Diff e1 e2) true

(/\) :: Tree FD a -> Tree FD b -> Tree FD b
(/\) = (>>)

(\/) :: Tree FD a -> Tree FD a -> Tree FD a
(\/) = Try

conj :: (Foldable t) => t (Tree FD ()) -> Tree FD ()
conj = foldl (/\) true

disj :: (Foldable t) => t (Tree FD ()) -> Tree FD ()
disj = foldl (\/) false

true :: (Solver solver) => Tree solver ()
true = Return ()

false :: (Solver solver) => Tree solver a
false = Fail
