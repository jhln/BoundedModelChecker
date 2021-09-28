{-# LANGUAGE TypeFamilies #-}

module Solver where

class Monad solver => Solver solver where
  type Constraint solver :: *
  type Term solver :: *
  type Label solver :: *
  newvar :: solver (Term solver)
  add :: Constraint solver -> solver Bool
  mark :: solver (Label solver)
  goto :: Label solver -> solver ()
  run :: solver a -> a
