{-# LANGUAGE TypeFamilies #-}
module Simple where

import FD
import Tree
import TreeSugar ((@+),
                  (@=),
                  (\/),
                  (/\),
                  exist,
                  assignments,
                  in_domain,
                  conj)

test :: Tree FD ()
test = NewVar $ \v -> (Add (Same (Var v) (Const 1))) $ Return ()

test2 :: Tree FD ()
test2 = NewVar $ \v -> (Try
                        (Add (Same (Var v) (Const 1)) (Return ()))
                        (Add (Same (Var v) (Const 2)) (Return ())))
test3 :: Tree FD ()
test3 = exist 3 (\vs -> model vs)

model vars = conj [ ((Var v) @= 1) \/ ((Var v) @= 2) | v <- vars ]

--test = do
    --x <- newVar [0..3]
    --y <- newVar [0..3]
    --(( x .<. y) \/ (x `same` y))
    --x `hasValue` 2

-- import NQueens, TreeSolver (FD, SOlver, Tree)

--staticBallsTest :: Tree FD ()
staticBallsTest = exist 10 (\vars -> ballsModel vars /\ assignments vars)

ballsModel terms = conj [ v `in_domain` (0,1) | v <- vars] /\
                   conj [ (v @= 0) \/ (v @= 1) | v <- vars] /\
                   ((b1_1 @+ b2_1 @+ b3_1 @+ r1_1 @+ r2_1) @= 1) /\
                   ((b1_2 @+ b2_2 @+ b3_2 @+ r1_2 @+ r2_2) @= 1)
  where
    [b1_1, b2_1, b3_1, r1_1, r2_1, b1_2, b2_2, b3_2, r1_2, r2_2] = vars
    vars = map Var terms
    blue1 = [b1_1, b2_1, b3_1]
    blue2 = [b1_2, b2_2, b3_2]
    blue = blue1 ++ blue2
    red1 = [r1_1, r2_1]
    red2 = [r1_2, r2_2]
    red = red1 ++ red2
ballsModel _ = Fail
