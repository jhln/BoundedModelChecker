module FD where

data FDConstraint s =
    Less (FDExpr s) (FDExpr s)
  | Diff (FDExpr s) (FDExpr s)
  | Same (FDExpr s) (FDExpr s)
  | Dom (FDExpr s) Int Int deriving (Show)


data FDExpr s = Var (FDTerm s)
  | Const Int
  | Plus (FDExpr s) (FDExpr s)
  | Minus (FDExpr s) (FDExpr s)
  | Mult (FDExpr s) (FDExpr s) deriving (Show)


data FDTerm s = Named String s | Generated Int s deriving (Show, Eq)
