{-# LANGUAGE TypeFamilies #-}
module NQueens where

import Data.List

import Tree
import Solver
--import TreeSolver
import FD

--nqueens n = exist n (\queens -> (model queens n))-- /\ (enumerate queens [1..n]))

--model queens n = (queens `allin` (1, n)) /\
--                 (alldifferent queens) /\
--                 (diagonals queens)

allin queens range = conj [q `in_domain` range | q <- queens ]


alldifferent queens = conj [ qi @/= qj | qi:qjs <- tails queens, qj <- qjs]

diagonals queens = conj [ (qi @/= (qj @+ (Const d))) /\
                          (qj @/= (qi @+ (Const d)))
                        | qi:qjs <- tails queens,
                          (qj,d) <- zip qjs ([1..]::[Int])]

enumerate queens values = conj [ enum queen values
                               | queen <- queens ]

enum :: FDExpr -> [Int] -> Tree Maybe ()
enum var values = disj [ var @= value | value <- values ]

{-
label [] = return ()
label (v:vs) = do
  d <- domain v
  return (enum v d /\ enumerate vs)
-}

-- Sugar

--conj :: (Foldable t) => (t (Tree solver a)) -> Tree solver a
conj = foldl (/\) true

--disj :: (Foldable t) => (t (Tree solver a)) -> Tree solver a
disj = foldl (\/) false

--exist :: Int -> ([Term solver] -> Tree solver a) -> Tree solver a
exist n k = f n []
  where
    f 0 acc = k acc
    f n acc = NewVar (\v -> f (n-1) (v:acc))


--in_domain :: Term solver -> (Int, Int) -> Tree solver a
v `in_domain` (l, u) = Add (Dom v l u) true

--(@=) :: Term solver -> Int -> Tree solver ()
e @= n = Add (Same e (Const n)) true

--(@/=) :: Term solver -> Term solver -> Tree solver a
e1 @/= e2 = Add (Diff e1 e2) true

--(@+) :: Term solver -> Term solver -> Term solver
e1 @+ e2 = Plus e1 e2

--true :: Tree solver ()
true = Return ()

--false :: Tree solver a
false = Fail

-- fail

--(/\) :: Tree solver a -> Tree solver b -> Tree solver b
(/\) = (>>)

(\/) = Try

instance Solver Maybe where
  type Constraint Maybe = FDConstraint
  type Term Maybe = FDTerm
  type Label Maybe = Int
  newvar = undefined
  add = undefined
  mark = undefined
  goto = undefined
  run = undefined
