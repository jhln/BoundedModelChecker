module BoundedTree where

import qualified LogicAssambler as LA
import BoundedSolver
import Tree


-- für pc=0
testTree1 :: Tree BMSolver ()
testTree1 = NewVar $ \v -> 
  Add 
    (Same 
      (Var v) 
      (Const 1))
    $ Return ()


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


testTree2 :: Tree BMSolver ()
testTree2 = NewVar $ \pc -> NewVar $ \i -> NewVar $ \r -> NewVar $ \n ->
            NewVar $ \pc' -> NewVar $ \i' -> NewVar $ \r' -> NewVar $ \n' ->
  Add 
    Same (Var pc) (Const 0) 
    $ Add 
        Same (Var i) (Const 0) 
        $ Add 
            Same (Var r) (Const 0) 
            $ Add 
                Same (Var n) (Const 0) 
                $ Add 
                    Same (Var pc') (Const 0) 
                    $ Add 
                        Same (Var i') (Const 0) 
                        $ Add 
                            Same (Var r') (Const 0) 
                            $ Add 
                                Same (Var n') (Const 0) 
$ (Try 
    Add 
      (Same (Var pc) (Const 0)) 
      $ Add 
        $ Same (Var i) BMSolverExpr (Tree solver a)
    (Try 
      (Tree solver a)
      (Try 
        (Tree solver a)
        (Try 
          (Tree solver a)
          (Try 
            (Tree solver a)
            (Try 
              (Tree solver a)
              (Tree solver a)))))))