module BoundedTree where

import qualified LogicAssambler as LA
import BoundedSolver
import Tree

{-
propsotion logic encoding:
pc=0 => i'=0, r'=0, n'=n, pc'=1
pc=1, i≤n => r'=r, i'=i, n'=n, pc'=2
pc=1, i>n => r′=r, n'=n, pc'=4
pc=2 => r'=r+1, i'=i, n'=n, pc'=3
pc=3 => r'=r, i'=i+1, n'=n, pc'=1
pc=4 => pc'=5 
pc=5 => pc'=5
-}


-- für pc=0
testTree1 :: Tree BMSolver ()
testTree1 = NewVar $ \v -> (Add (Same (Var v) (Const 1))) $ Return ()