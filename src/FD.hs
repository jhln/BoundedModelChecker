module FD where

data FDConstraint
  = Less FDExpr FDExpr
  | Diff FDExpr FDExpr
  | Same FDExpr FDExpr
  | Dom FDExpr Int Int deriving (Show)


data FDExpr
  = Var FDTerm
  | Const Int
  | Plus FDExpr FDExpr
  | Minus FDExpr FDExpr
  | Mult FDExpr FDExpr  deriving (Show)


data FDTerm = Named String | Generated Int deriving (Show, Eq)
