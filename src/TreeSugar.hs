{-# LANGUAGE TypeFamilies #-}
module TreeSugar where

import Tree
import Solver
import FD

import           Control.Monad.State.Lazy

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Map (Map, (!))



exist n k = f n []
  where
    f 0 acc = k acc
    f m acc = NewVar (\v -> f (m-1) (v:acc))

(@=) :: FDExpr -> Int -> Tree FD ()
e @= n = Add (Same e (Const n)) true

(@+) :: FDExpr -> FDExpr -> FDExpr
e1 @+ e2 = Plus e1 e2

in_domain :: FDExpr -> (Int, Int) -> Tree FD ()
in_domain v (l, u) = Add (Dom v l u ) true

--(@/=) :: Term solver -> Term solver -> Tree solver a
e1 @/= e2 = Add (Diff e1 e2) true

(/\) :: (Solver solver) => Tree solver a -> Tree solver b -> Tree solver b
(/\) = (>>)

(\/) :: (Solver solver) => Tree solver a -> Tree solver a -> Tree solver a
(\/) = Try

conj :: (Foldable t) => t (Tree FD ()) -> Tree FD ()
conj = foldl (/\) true

disj :: (Foldable t) => t (Tree FD ()) -> Tree FD ()
disj = foldl (\/) false

true :: (Solver solver) => Tree solver ()
true = Return ()

false :: (Solver solver) => Tree solver a
false = Fail


assignments :: [FDTerm] -> Tree FD [Int]
assignments = mapM assignment

assignment :: FDTerm -> Tree FD Int
assignment q = Dynamic $ value q >>= (return . Return)

value :: FDTerm -> FD Int
value var = do
  s <- get
  let vm = varMap s
  let vi = vm ! v
  let d = domain vi
  return (IntSet.findMin d)
  where
    (FDTerm v) = var
