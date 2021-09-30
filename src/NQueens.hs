{-# LANGUAGE TypeFamilies #-}
module NQueens where

import           Data.List

import           Tree
import           Solver
--import TreeSolver
import           FD
import           TreeSugar

import           Control.Monad
import           Control.Monad.State.Lazy

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Map (Map, (!))



--nqueens n = exist n (\queens -> (model queens n))-- /\ (enumerate queens [1..n]))

--model queens n = (queens `allin` (1, n)) /\
--                 (alldifferent queens) /\
--                 (diagonals queens)

nrooksSimple n = exist n (\rooks -> (modelSimple rooks n))

modelSimple rooks n = (rooks `allin` (1, n)) /\
                      alldifferent rooks

nrooksEnumerate n = exist n (\rooks -> (modelSimple rooks n) /\
                              (enumerate rooks [1..n]))

nrooksDynamic n = exist n (\rooks -> modelSimple rooks n /\
                            enumerateDynamic rooks)

nrooksAssignments n = exist n (\rooks -> modelSimple rooks n /\
                              enumerateDynamic rooks /\
                              assignments rooks)

enumerateDynamic = Dynamic . label

allin :: [FDTerm] -> (Int, Int) -> Tree FD ()
allin queens range = conj [(Var q) `in_domain` range | q <- queens ]


alldifferent queens = conj [ (Var qi) @/= (Var qj)
                           | qi:qjs <- tails queens, qj <- qjs]


{-
diagonals queens = conj [ (qi @/= (qj @+ (Const d))) /\
                          (qj @/= (qi @+ (Const d)))
                        | qi:qjs <- tails queens,
                          (qj,d) <- zip qjs ([1..]::[Int])]
-}

enumerate queens values = conj [ enum queen values
                               | queen <- queens ]

assignments :: [FDTerm] -> Tree FD [Int]
assignments = mapM assignment

assignment :: FDTerm -> Tree FD Int
assignment q = Dynamic (value q >>= (return . Return))

enum :: FDTerm -> [Int] -> Tree FD ()
enum var values = disj [ (Var var) @= value | value <- values ]

label :: [FDTerm] -> FD (Tree FD ())
label [] = return (Return ())
label (v:vs) = do
--  let vm = varMap vm
  s <- get
  let vm = varMap s
  let vi = vm ! (unFDTerm v)
  let d = domain vi
  return (enum v (IntSet.toList d) /\ enumerateDynamic vs)
  {-do
  d <- (domain . unFDTerm) v
-}

value :: FDTerm -> FD Int
value var = do
  s <- get
  let vm = varMap s
  let vi = vm ! v
  let d = domain vi
  return (IntSet.findMin d)
  where
    (FDTerm v) = var
