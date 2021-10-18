{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FD where

import           Solver

import           Prelude hiding (lookup, sum)

import           Control.Applicative hiding (Const)
import           Control.Monad
import           Control.Monad.State.Lazy

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Maybe

data FDConstraint
  = Less FDExpr FDExpr
  | Diff FDExpr FDExpr
  | Same FDExpr FDExpr
  | Dom FDExpr Int Int deriving (Show)


data FDExpr
  = Var FDTerm
  | Const Int
  | Plus FDExpr FDExpr
--  | Minus FDExpr FDExpr
--  | Mult FDExpr FDExpr
  deriving (Show)


newtype FDTerm = FDTerm { unFDTerm :: FDVar } deriving (Show, Eq)

instance Solver FD where
  type Constraint FD  = FDConstraint
  type Term       FD  = FDTerm
  type Label      FD  = FDState

  newvar  = newVar [-10..10] >>= (return . FDTerm)
  add     = addC
  run p    = runFD p

  mark = get
  goto = put


-- The FD monad
newtype FD a = FD { unFD :: StateT FDState Maybe a }
    deriving (MonadState FDState)

deriving instance Functor FD

deriving instance Applicative FD

deriving instance Monad FD

deriving instance Alternative FD

deriving instance MonadPlus FD

addC :: FDConstraint -> FD Bool
addC (Less (Var v1) (Var v2)) = unFDTerm v1 .<. unFDTerm v2
addC (Less (Const i) (Var v)) = unFDTerm v `greater` i
addC (Less (Var v) (Const i)) = unFDTerm v `less` i
addC (Same (Var v1) (Var v2)) = unFDTerm v1 `same` unFDTerm v2
addC (Same (Var v) (Const c)) = unFDTerm v `hasValue` c
addC (Same (Const c) (Var v)) = unFDTerm v `hasValue` c
addC (Same (Const c1) (Const c2)) = return (c1 == c2)
addC (Same (Plus (Var v1) (Var v2)) (Const c)) = plus c (unFDTerm v1) (unFDTerm v2)
addC (Same p@(Plus _ _) (Const c)) = sum c collected
  where
    collected = (map unFDTerm . collect) p
    collect (Plus (Var v1) (Var v2)) = [v1, v2]
    collect (Plus (Var v) p') = v:collect p'
    collect (Plus p' (Var v)) = v:collect p'
    collect (Plus p1 p2) = collect p1 ++ collect p2
addC (Diff (Var v1) (Var v2)) = unFDTerm v1 `different` unFDTerm v2
addC (Dom v l u)              = addC (Less v (Const (u + 1))) >>
                                addC (Less (Const (l - 1)) v)


-- FD variables
newtype FDVar = FDVar { unFDVar :: Int } deriving (Ord, Eq, Show)

type VarSupply = FDVar

data VarInfo = VarInfo
     { delayedConstraints :: FD Bool, domain :: IntSet }

instance Show VarInfo where
  show x = show $ domain x

type VarMap = Map FDVar VarInfo

data FDState = FDState
     { varSupply :: VarSupply,
       varMap :: VarMap--,
     --  objective :: FDVar
     }
     deriving Show



-- Run the FD monad and produce a lazy list of possible solutions.
runFD :: FD a -> a
runFD fd = fromJust $ evalStateT (unFD fd') initState
           where fd' = fd -- fd' = newVar () >> fd

initState :: FDState
initState = FDState { varSupply = FDVar 0,
                      varMap = Map.empty--,
                      --objective = FDVar 0
                    }


-- Get a new FDVar
newVar :: [Int] -> FD FDVar
newVar d = do
    s <- get
    let v = varSupply s
    put $ s { varSupply = FDVar (unFDVar v + 1) }
    modify $ \s ->
        let vm = varMap s
            vi = VarInfo {
                delayedConstraints = return True,
                domain = IntSet.fromList d}
        in
        s { varMap = Map.insert v vi vm }
    return v

newVars :: Int -> [Int] -> FD [FDVar]
newVars n d = replicateM n (newVar d)


-- Lookup the current domain of a variable.
lookup :: FDVar -> FD IntSet
lookup x = do
    s <- get
    return . domain $ varMap s ! x

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar -> IntSet -> FD Bool
update x i = do
--    trace (show x ++ " <- " ++ show i)  (return ())
    s <- get
    let vm = varMap s
    let vi = vm ! x
--    trace ("where old domain = " ++ show (domain vi)) (return ())
    put $ s { varMap = Map.insert x (vi { domain = i}) vm }
    delayedConstraints vi

-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar -> FD Bool -> FD ()
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap =
        Map.insert x (vi { delayedConstraints = do b <- cs
                                                   if b then constraint
                                                        else return False}) vm }

-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint = FDVar -> FDVar -> FD Bool
addBinaryConstraint :: BinaryConstraint -> BinaryConstraint
addBinaryConstraint f x y = do
    let constraint  = f x y
    b <- constraint
    when b $ (do addConstraint x constraint
                 addConstraint y constraint)
    return b

addListConstraint :: ([FDVar] -> FD Bool) -> [FDVar] -> FD Bool
addListConstraint f ls = do
  let constraint = f ls
  b <- constraint
  when b $! mapM_ (`addConstraint` constraint) ls
  return b

-- Constrain a variable to a particular value.
hasValue :: FDVar -> Int -> FD Bool
var `hasValue` val = do
    vals <- lookup var
    if val `IntSet.member` vals
       then do let i = IntSet.singleton val
               if (i /= vals)
                  then update var i
                  else return True
       else return False

-- Constrain two variables to have the same value.
same :: FDVar -> FDVar -> FD Bool
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = xv `IntSet.intersection` yv
    if not $ IntSet.null i
       then whenwhen (i /= xv)  (i /= yv) (update x i) (update y i)
       else return False

whenwhen c1 c2 a1 a2  =
  if c1
     then do b1 <- a1
             if b1
                then if c2
                        then a2
                        else return True
                else return False
     else if c2
             then a2
             else return True


-- Constrain two variables to have different values.
different :: FDVar  -> FDVar  -> FD Bool
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
allDifferent :: [FDVar ] -> FD  ()
allDifferent (x:xs) = do
    mapM_ (different x) xs
    allDifferent xs
allDifferent _ = return ()


-- Constrain one variable to have a value less than the value of another
-- variable.
infix 4 .<.
(.<.) :: FDVar -> FDVar -> FD Bool
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

greater :: FDVar -> Int -> FD Bool
greater x i = do
  xv <- lookup x
  let xv' = IntSet.filter (> i) xv
  if not $ IntSet.null xv'
    then (update x xv')
    else return False

less :: FDVar -> Int -> FD Bool
less x i = do
  xv <- lookup x
  let xv' = IntSet.filter (< i) xv
  if not $ IntSet.null xv'
    then (update x xv')
    else return False

plus :: Int -> FDVar -> FDVar -> FD Bool
plus i = addBinaryConstraint $! \x y -> do
      xv <- lookup x
      yv <- lookup y
      let xv' = IntSet.filter (\(x' :: IntSet.Key) -> anyIS (\y' -> x' + y' == i) yv ) xv
      let yv' = IntSet.filter (\(y' :: IntSet.Key) -> anyIS (\x' -> x' + y' == i) xv') yv
      if not (IntSet.null xv' || IntSet.null yv')
        then whenwhen (xv /= xv') (yv /= yv') (update x xv') (update y yv')
        else return False

anyIS :: (IntSet.Key -> Bool) -> IntSet -> Bool
anyIS f = any f . IntSet.elems

sum :: Int -> [FDVar] -> FD Bool
sum i = addListConstraint sum'
  where
    sum' :: [FDVar] -> FD Bool
    sum' [] = return True
    sum' [x] = x `hasValue` i
    sum' [x,y] = plus i x y
    sum' [x,y,z] = do
      xv <- lookup x
      yv <- lookup y
      zv <- lookup z
      let xv' = IntSet.filter (\x' -> anyIS (\y' -> anyIS (\z' -> x' + y' + z' == i) zv) yv) xv
      let yv' = IntSet.filter (\y' -> anyIS (\x' -> anyIS (\z' -> x' + y' + z' == i) zv) xv') yv
      let zv' = IntSet.filter (\z' -> anyIS (\x' -> anyIS (\y' -> x' + y' + z' == i) yv') xv') zv
      if not (IntSet.null xv' || IntSet.null yv' || IntSet.null zv')
        then do
        xu <- if xv' /= xv
              then
                update x xv'
              else
                return True
        yu <- if yv' /= yv
              then
                update y yv'
              else
                return True
        zu <- if zv' /= zv
              then
                update z zv'
              else
                return True
        return (xu && yu && zu)
        else
        return False
    sum' [a, b, c, d] = do
      av <- lookup a
      bv <- lookup b
      cv <- lookup c
      dv <- lookup d
      let av' = IntSet.filter (\a' -> anyIS (\b' -> anyIS (\c' -> anyIS (\d' -> a' + b' + c' + d' == i) dv) cv) bv) av
      let bv' = IntSet.filter (\b' -> anyIS (\c' -> anyIS (\d' -> anyIS (\a' -> a' + b' + c' + d' == i) av') dv) cv) bv
      let cv' = IntSet.filter (\c' -> anyIS (\d' -> anyIS (\a' -> anyIS (\b' -> a' + b' + c' + d' == i) bv') av') dv) cv
      let dv' = IntSet.filter (\d' -> anyIS (\a' -> anyIS (\b' -> anyIS (\c' -> a' + b' + c' + d' == i) cv') bv') av') dv
      if not (IntSet.null av' || IntSet.null bv' || IntSet.null cv' || IntSet.null dv')
        then do
        au <- if av' /= av
          then
          update a av'
          else
          return True
        bu <- if bv' /= bv
          then
          update b bv'
          else
          return True
        cu <- if cv' /= cv
          then
          update c cv'
          else
          return True
        du <- if dv' /= dv
          then
          update d dv'
          else
          return True
        return (au && bu && cu && du)
        else
        return False
    sum' [a, b, c, d, e] = do
      av <- lookup a
      bv <- lookup b
      cv <- lookup c
      dv <- lookup d
      ev <- lookup d
      let av' = IntSet.filter (\a' -> anyIS (\b' -> anyIS (\c' -> anyIS (\d' -> anyIS (\e' -> a' + b' + c' + d' + e' == i) ev) dv) cv) bv) av
      let bv' = IntSet.filter (\b' -> anyIS (\a' -> anyIS (\c' -> anyIS (\d' -> anyIS (\e' -> a' + b' + c' + d' + e' == i) ev) dv) cv) av') bv
      let cv' = IntSet.filter (\c' -> anyIS (\b' -> anyIS (\a' -> anyIS (\d' -> anyIS (\e' -> a' + b' + c' + d' + e' == i) ev) dv) av') bv') cv
      let dv' = IntSet.filter (\d' -> anyIS (\b' -> anyIS (\c' -> anyIS (\a' -> anyIS (\e' -> a' + b' + c' + d' + e' == i) ev) av') cv') bv') dv
      let ev' = IntSet.filter (\e' -> anyIS (\b' -> anyIS (\c' -> anyIS (\d' -> anyIS (\a' -> a' + b' + c' + d' + e' == i) av') dv') cv') bv') ev
      if not (IntSet.null av' || IntSet.null bv' || IntSet.null cv' || IntSet.null dv' || IntSet.null ev')
        then do
        au <- if av' /= av
          then
          update a av'
          else
          return True
        bu <- if bv' /= bv
          then
          update b bv'
          else
          return True
        cu <- if cv' /= cv
          then
          update c cv'
          else
          return True
        du <- if dv' /= dv
          then
          update d dv'
          else
          return True
        eu <- if ev' /= ev
          then
          update e ev'
          else
          return True
        return (au && bu && cu && du && eu)
        else
        return False
