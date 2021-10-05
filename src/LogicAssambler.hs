{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module LogicAssambler where

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

newtype LAProg = LProg [LA]

instance Show LAProg where
  show (LProg la) = concatMap (\rule -> show rule ++ "\n") la

data LA = Rule Conds Conseqs
instance Show LA where
  show (Rule conds conseqs) 
    = "Rule" ++ "\n" 
    ++ "Conds: " ++ show conds ++ "\n"
    ++ "Conseqs: " ++ show conseqs


type Conds = [Alloc]
type Conseqs = [Alloc]

data Alloc
  = IS Var (Either Var Val)
  | LE Var (Either Var Val)
  | GR Var (Either Var Val)
  deriving (Show)
newtype Var = Var String deriving Show
data Val
  = Val Integer
  | Inc Var
  deriving Show

data AllocDet = H | L | G

p1 :: LA
p1 = Rule
      [IS (Var "pc") (Right (Val 0))]
      [IS (Var "i") (Right (Val 0))
      ,IS (Var "r") (Right (Val 0))
      ,IS (Var "n") (Left (Var "n"))
      ,IS (Var "pc") (Right (Val 1))]

laProg1 :: LAProg
laProg1 = LProg $ map makeRule [l1,l2,l3,l4,l5,l6,l7]

makeRule :: ([(String, AllocDet , Either String Integer)], [(String, Either String Integer)]) -> LA
makeRule (conds,conseqs) = Rule (map makeAlloc conds) (map makeAlloc insertedDet)
  where
    insertedDet = map (\(string, either) -> (string, H, either)) conseqs

makeAlloc :: (String, AllocDet, Either String Integer) -> Alloc
makeAlloc (v,det,assig) = constructor (Var v) (inject assig)
  where
    constructor :: Var -> Either Var Val -> Alloc
    constructor = case det of
      H -> IS
      L -> LE
      G -> GR
    inject :: Either String Integer -> Either Var Val
    inject (Left s) = Left $ Var s
    inject (Right (-1)) = Right $ Inc (Var v)
    inject (Right i) = Right $ Val i


l1:: ([(String, AllocDet , Either String Integer)], [(String, Either String Integer)])
l1 =  ( [("pc",H, Right 0)]
      , [("i", Right 0), ("r", Right 0), ("n", Left "n"), ("pc", Right 1)])
l2:: ([(String, AllocDet , Either String Integer)], [(String, Either String Integer)])
l2 =  ( [("pc",H, Right 1), ("i", L, Left "n")]
      , [("r",Left "r"), ("i",Left "i"), ("n",Left "n"), ("pc",Right 2)])
l3:: ([(String, AllocDet , Either String Integer)], [(String, Either String Integer)])
l3 =  ( [("pc",H,Right 1), ("i",G,Left "n") ]
      , [("r",Left "r"), ("n", Left "n"), ("pc",Right 4)])
l4:: ([(String, AllocDet , Either String Integer)], [(String, Either String Integer)])
l4 =  ( [("pc",H, Right 2)]
      , [("r",Right $ -1), ("i",Left "i"), ("n",Left "n"), ("pc",Right 3)])
l5:: ([(String, AllocDet , Either String Integer)], [(String, Either String Integer)])
l5 =  ( [("pc",H,Right 3)]
      , [("r",Left "r"), ("i",Right (-1)), ("n",Left "n"), ("pc",Right 1)])
l6:: ([(String, AllocDet , Either String Integer)], [(String, Either String Integer)])
l6 =  ( [("pc",H,Right 4)]
      , [("pc",Right 5)])
l7:: ([(String, AllocDet , Either String Integer)], [(String, Either String Integer)])
l7 =  ( [("pc",H, Right 5)]
      , [("pc",Right 5)])

