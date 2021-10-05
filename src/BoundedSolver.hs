{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module BoundedSolver where


import           Solver

import           Prelude hiding (lookup)

import           Control.Applicative hiding (Const)
import           Control.Monad
import           Control.Monad.State.Lazy

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Maybe

-- Constraints angepasst
data BMSolverConstraint
  = Less BMSolverExpr BMSolverExpr
  | Same BMSolverExpr BMSolverExpr
--  | Inc BMSolverExpr BMSolverExpr
  deriving (Show)


data BMSolverExpr
  = Var BMSolverTerm
  | Const Int
--  | Plus BMSolverExpr BMSolverExpr
--  | Minus BMSolverExpr BMSolverExpr
--  | Mult BMSolverExpr BMSolverExpr
  deriving (Show)


newtype BMSolverTerm = BMSolverTerm { unBMSolverTerm :: BMSolverVar } deriving (Show, Eq)

instance Solver BMSolver where
  type Constraint BMSolver  = BMSolverConstraint
  type Term       BMSolver  = BMSolverTerm
  type Label      BMSolver  = BMSolverState

  newvar  = newVar [-10..10] >>= (return . BMSolverTerm)
  add     = addC
  run p    = runBMSolver p

  mark = get
  goto = put


-- The BMSolver monad
newtype BMSolver a = BMSolver { unBMSolver :: StateT BMSolverState Maybe a }
    deriving (MonadState BMSolverState)

deriving instance Functor BMSolver

deriving instance Applicative BMSolver

deriving instance Monad BMSolver

deriving instance Alternative BMSolver

deriving instance MonadPlus BMSolver

addC :: BMSolverConstraint -> BMSolver Bool
addC (Less (Var v1) (Var v2)) = (u BMSolverTerm v1) .<. (u BMSolverTerm v2)
addC (Less (Const i) (Var v)) = (u BMSolverTerm v) `greater` i
addC (Less (Var v) (Const i)) = (u BMSolverTerm v) `less` i
addC (Same (Var v1) (Var v2)) = (u BMSolverTerm v1) `same` (u BMSolverTerm v2)
addC (Same (Var v) (Const c)) = (u BMSolverTerm v) `hasValue` c
addC (Same (Const c) (Var v)) = (u BMSolverTerm v) `hasValue` c


-- BMSolver variables
newtype BMSolverVar = BMSolverVar { unBMSolverVar :: Int } deriving (Ord, Eq, Show)

type VarSupply = BMSolverVar

data VarInfo = VarInfo
     { delayedConstraints :: BMSolver Bool, domain :: IntSet }

instance Show VarInfo where
  show x = show $ domain x

type VarMap = Map BMSolverVar VarInfo

data BMSolverState = BMSolverState
     { varSupply :: VarSupply,
       varMap :: VarMap--,
     --  objective :: BMSolverVar
     }
     deriving Show



-- Run the BMSolver monad and produce a lazy list of possible solutions.
runBMSolver :: BMSolver a -> a
runBMSolver BMSolver = fromJust $ evalStateT (unBMSolver BMSolver') initState
           where BMSolver' = BMSolver -- BMSolver' = newVar () >> BMSolver

initState :: BMSolverState
initState = BMSolverState { varSupply = BMSolverVar 0,
                      varMap = Map.empty--,
                      --objective = BMSolverVar 0
                    }


-- Get a new BMSolverVar
newVar :: [Int] -> BMSolver BMSolverVar
newVar d = do
    s <- get
    let v = varSupply s
    put $ s { varSupply = BMSolverVar (unBMSolverVar v + 1) }
    modify $ \s ->
        let vm = varMap s
            vi = VarInfo {
                delayedConstraints = return True,
                domain = IntSet.fromList d}
        in
        s { varMap = Map.insert v vi vm }
    return v

newVars :: Int -> [Int] -> BMSolver [BMSolverVar]
newVars n d = replicateM n (newVar d)


-- Lookup the current domain of a variable.
lookup :: BMSolverVar -> BMSolver IntSet
lookup x = do
    s <- get
    return . domain $ varMap s ! x

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: BMSolverVar -> IntSet -> BMSolver Bool
update x i = do
--    trace (show x ++ " <- " ++ show i)  (return ())
    s <- get
    let vm = varMap s
    let vi = vm ! x
--    trace ("where old domain = " ++ show (domain vi)) (return ())
    put $ s { varMap = Map.insert x (vi { domain = i}) vm }
    delayedConstraints vi

-- Add a new constraint for a variable to the constraint store.
addConstraint :: BMSolverVar -> BMSolver Bool -> BMSolver ()
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap =
        Map.insert x (vi { delayedConstraints = do b <- cs
                                                   if b then constraint
                                                        else return False}) vm }

-- Useful helper function for adding binary constraints between BMSolverVars.
type BinaryConstraint = BMSolverVar -> BMSolverVar -> BMSolver Bool
addBinaryConstraint :: BinaryConstraint -> BinaryConstraint
addBinaryConstraint f x y = do
    let constraint  = f x y
    b <- constraint
    when b $ (do addConstraint x constraint
                 addConstraint y constraint)
    return b


-- Constrain a variable to a particular value.
hasValue :: BMSolverVar -> Int -> BMSolver Bool
var `hasValue` val = do
    vals <- lookup var
    if val `IntSet.member` vals
       then do let i = IntSet.singleton val
               if (i /= vals)
                  then update var i
                  else return True
       else return False

-- Constrain two variables to have the same value.
same :: BMSolverVar -> BMSolverVar -> BMSolver Bool
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = xv `IntSet.intersection` yv
    if not $ IntSet.null i
       then whenwhen (i /= xv)  (i /= yv) (update x i) (update y i)
       else return False

whenwhen c1 c2 a1 a2
  | c1 = do b1 <- a1
            if b1
               then if c2
                       then a2
                       else return True
               else return False
  | c2 = a2
  | otherwise = return True


-- Constrain two variables to have different values.
different :: BMSolverVar  -> BMSolverVar  -> BMSolver Bool
different = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    if not (isSingleton xv) || not (isSingleton yv) || xv /= yv
       then whenwhen (isSingleton xv && xv `IntSet.isSubsetOf` yv)
                     (isSingleton yv && yv `IntSet.isSubsetOf` xv)
                     (update y (yv `IntSet.difference` xv))
                     (update x (xv `IntSet.difference` yv))
       else return False
      where
        isSingleton = (== 1) . IntSet.size

-- Constrain a list of variables to all have different values.
allDifferent :: [BMSolverVar ] -> BMSolver  ()
allDifferent (x:xs) = do
    mapM_ (different x) xs
    allDifferent xs
allDifferent _ = return ()


-- Constrain one variable to have a value less than the value of another
-- variable.
infix 4 .<.
(.<.) :: BMSolverVar -> BMSolverVar -> BMSolver Bool
(.<.) = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let xv' = IntSet.filter (< IntSet.findMax yv) xv
    let yv' = IntSet.filter (> IntSet.findMin xv) yv
    if  not $ IntSet.null xv'
        then if not $ IntSet.null yv'
                then whenwhen (xv /= xv') (yv /= yv') (update x xv') (update y yv')
                else return False
        else return False

greater :: BMSolverVar -> Int -> BMSolver Bool
greater x i = do
  xv <- lookup x
  let xv' = IntSet.filter (> i) xv
  if not $ IntSet.null xv'
    then update x xv'
    else return False

less :: BMSolverVar -> Int -> BMSolver Bool
less x i = do
  xv <- lookup x
  let xv' = IntSet.filter (< i) xv
  if not $ IntSet.null xv'
    then update x xv'
    else return False
