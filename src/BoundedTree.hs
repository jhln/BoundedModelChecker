{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module BoundedTree where

import qualified LogicAssambler as LA
import BoundedSolver
import Tree
import TreeSugar


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
testTree2 = NewVar (\(pc ::BMSolverTerm) ->
            NewVar (\(pc'::BMSolverTerm) ->
            NewVar (\(n  ::BMSolverTerm) ->
            NewVar (\(n' ::BMSolverTerm) ->
                      model [pc, pc', n, n'] transition property))))
  where
    {-
      We try to extract 'property' and 'transition' from the SMTLib Format (or
      from LogicAssembler) This is the program:

      n = 0 (pc = 0)
      n = 1 (pc = 1)
      assert (n == 1)
    -}
    property :: [BMSolverTerm] -> Tree BMSolver ()
    property [_, pc', _, n'] = (pc' #= 2) /\ (n' #/= 1)
    transition :: [BMSolverTerm] -> Int -> Tree BMSolver ()
    transition [pc, pc', n, n'] 0 = (pc #= 0) /\ -- (in the assembler this is => but we only create one 'State' at a time)
                                    (pc' #= 1) /\
                                    (n #= 0) /\
                                    (n' #= 0)
    transition [pc, pc', _, n'] 1 = (pc #= 1) /\
                                    (pc' #= 2) /\
                                    (n' #= 1)
    transition [pc, pc', _, _] 2 = (pc #= 2) /\ (pc' #= 2) -- end of program
    (#=) :: BMSolverTerm -> Int -> Tree BMSolver ()
    (#=) t i = Add (Same (Var t) (Const i)) true
    {- Cannot be implemented correctly yet -}
    (#/=) t i = Add (Same (Var t) (Const (i-1))) true
    model vs t p = Try ((t vs 0) /\ p vs)
                   (Dynamic (return
                             (Try (t vs 1 /\ p vs)
                              (Dynamic (return (Try (t vs 2 /\ p vs) Fail))))))

{-
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
-}
