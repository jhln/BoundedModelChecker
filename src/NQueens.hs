{-# LANGUAGE TypeFamilies #-}
module NQueens where

import           Data.List

import           Tree
import           Solver
import           TreeSolver
import           FD
import           TreeSugar

import           Control.Monad
import           Control.Monad.State.Lazy

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Map (Map, (!))
import           Sets
--import           Relations

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

oneChoisesFromKBalls k =
  exist 1 (\choices ->
              modelSimple choices k
              /\ enumerateDynamic choices --- is this necessary ?
              /\ assignments choices)

nChoisesFromKBalls n k =
  exist n (\choices ->
              modelSimple choices k
              /\ enumerateDynamic choices --- is this necessary ?
              /\ assignments choices)

test1ChoicesFrom2Balls = solve (oneChoisesFromKBalls 2)
testNChoicesFormKBalls n k = solve $ nChoisesFromKBalls n k
test3ChoicesForm5Balls :: [[Int]]
test3ChoicesForm5Balls = solve $ nChoisesFromKBalls 3 5


--- Partition wenn Reihenfolge egal
--- BUT might be wrong !!!!

setOfChoicePaths :: Ord a => [a] -> Set a
setOfChoicePaths = foldr insertSet emptySet
listOfChoices :: Ord a => [[a]] -> [Set a]
listOfChoices = map (foldr insertSet emptySet)

setOfSetOfChoices1 :: Ord a => [[a]] -> [Set a]
setOfSetOfChoices1 = listOfChoices
setOfSetOfChoices2 :: Ord a => [Set a] -> Set (Set a)
setOfSetOfChoices2 = setOfChoicePaths
setOfSetOfChoices :: Ord a => [[a]] -> Set (Set a)
setOfSetOfChoices = setOfChoicePaths . listOfChoices

setOfSetChoicesFold :: Ord a => [[a]] -> Set (Set a)
setOfSetChoicesFold = foldr insertSet emptySet
                    . map (foldr insertSet emptySet)
setOfSetChoicesFuse :: Ord a => [[a]] -> Set (Set a)
setOfSetChoicesFuse =
  foldr
    (insertSet . foldr
                    insertSet
                    emptySet)
    emptySet

kOverN k n = setOfSetChoicesFuse
           $ solve $ nChoisesFromKBalls n k

-------------------------------------------------------


tBlueAndsRedFromBallSetWithNChoices t s n ballsList =
  let
    ballSetMemberNameList = [i | (i,c) <- ballsList]
    ballSetOrderingRelation =
      zipWith
        (\i count -> (count,i))
        ballSetMemberNameList
        [1..]
    possibleChoices =
      testNChoicesFormKBalls n $ length ballSetOrderingRelation
    possibleChoicesWithoutOrder =
      setOfSetChoicesFuse possibleChoices
    -- Relation composition

    -- Equivalence relation
  in
    possibleChoices

-------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------


--- Blue, Red Assignment
data Color = Blue
           | Red
           deriving (Show, Eq, Ord)

balls :: [(Int, Color)]
balls = [(n,Blue) | n <- [1..3]] ++ [(n,Red) | n <- [4,5]]
ballsSet :: Set (Int, Color)
ballsSet = foldr insertSet emptySet balls
ballsRel :: DirectedRel Int Color
ballsRel = ballsSet

--- Directed Relations

type DirectedRel a b = Set (a, b)

projDom :: DirectedRel a b -> DirectedRel Integer a
projDom (Set pairs) = 
  Set $ zipWith (\ i (a,b) -> (i,a)) [1..] pairs
projRange :: DirectedRel a b -> DirectedRel Integer b
projRange (Set pairs) = 
  Set $ zipWith (\ i (a,b) -> (i,b)) [1..] pairs

codomainForDomElem :: Eq a => 
  a -> DirectedRel a b -> [b]
codomainForDomElem e (Set pairs) = 
  [b | (a,b) <- pairs, a == e]
domainForCodomElem :: Eq b => 
  b -> DirectedRel a b -> [a]
domainForCodomElem e (Set pairs) = 
  [a | (a,b) <- pairs, b == e]

countColoured = do
  choices <- test3ChoicesForm5Balls
  --return choices
  t <- return $ do
    chocenBall <- choices
    codomainForDomElem chocenBall ballsSet
  let selection = countCol t (0,0)
   --countEqPair (2,1) selection
  return selection

test nrB nrR = countEqPair (nrB,nrR) countColoured 0
test2Blues1Red = test 2 1


countCol :: (Num a, Num b) => [Color] -> (a, b) -> (a, b)
countCol (Blue:rest) (nrB, nrR) = countCol rest (nrB+1,nrR)
countCol (Red:rest) (nrB, nrR) = countCol rest (nrB,nrR+1)
countCol [] (nrB, nrR) = (nrB,nrR)

countEqPair :: (Eq a, Eq b) => (a, b) -> [(a, b)] -> Integer -> Integer
countEqPair (nrB,nrR) ((nrB', nrR'):rest) acc
  | nrB == nrB' && nrR == nrR' = 
    countEqPair (nrB,nrR) rest $  acc +1
  | otherwise = countEqPair (nrB,nrR) rest acc
countEqPair _ [] acc = acc



listPartition :: Eq a => [a] -> [[a]] -> Bool
listPartition xs xss =
  all (`Sets.elem` xs) (concat xss)
  && all (`Sets.elem` concat xss) xs
  && listPartition' xss []
    where
      listPartition' [] _ = True
      listPartition' ([]:xss) _ = False
      listPartition' (xs:xss) domain
        | null (Sets.intersect xs domain) =
            listPartition' xss (Sets.union xs domain)
        | otherwise = False


-- domR gives the domain of a relation.
domR :: Ord a => DirectedRel a b -> Set a
domR (Set r) = list2set [ x | (x,_) <- r ]

-- ranR gives the range of a relation.
ranR :: (Ord a, Ord b) => DirectedRel a b -> Set b
ranR (Set r) = list2set [ y | (_,y) <- r ]

-- idR creates the identity relation ∆A over a set A:
idR :: Ord a => Set a -> DirectedRel a a
idR (Set xs) = Set [(x,x) | x <- xs]

-- The total relation over a set is given by:
totalR :: Set a -> DirectedRel a a
totalR (Set xs) = Set [(x,y) | x <- xs, y <- xs ]

-- invR inverts a relation (i.e., the function maps R to R−1
invR :: (Ord a, Ord b) => DirectedRel a b -> DirectedRel b a
invR (Set []) = Set []
invR (Set ((x,y):r)) = insertSet (y,x) (invR (Set r))

-- inR checks whether a pair is in a relation.
inR :: (Ord a, Ord b) => DirectedRel a b -> (a,b) -> Bool
inR r (x,y) = inSet (x,y) r

-- complement of a relation R
complR :: Ord a => Set a -> DirectedRel a a -> DirectedRel a a
complR (Set xs) r =
  Set [ (x,y) | x <- xs
              , y <- xs
              , not (inR r (x,y))]

--- bilds a combined relation from a pair and another relation
composePair :: (Ord a, Eq b, Ord c) =>
  (a,b) -> DirectedRel b c -> DirectedRel a c
composePair (x,y) (Set []) = Set []
composePair (x,y) (Set ((u,v):s))
  | y == u = insertSet (x,v) (composePair (x,y) (Set s))
  | otherwise = composePair (x,y) (Set s)

--- unites 2 sets
unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (Set []) set2 = set2
unionSet (Set (x:xs)) set2 =
  insertSet x (unionSet (Set xs) (deleteSet x set2))

--- Composing 2 Relations
compR :: (Ord a, Eq b, Ord c)=>
  DirectedRel a b -> DirectedRel b c -> DirectedRel a c
compR (Set []) _ = Set []
compR (Set ((x,y):s)) r =
  unionSet (composePair (x,y) r) (compR (Set s) r)














enumerateDynamic = Dynamic . label

allin :: [FDTerm] -> (Int, Int) -> Tree FD ()
allin queens range = conj [Var q `in_domain` range | q <- queens ]


alldifferent queens = conj [  Var qi @/= Var qj
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
assignment q = Dynamic $ value q >>= (return . Return)

enum :: FDTerm -> [Int] -> Tree FD ()
enum var values = disj [  Var var @= value | value <- values ]

label :: [FDTerm] -> FD (Tree FD ())
label [] = return (Return ())
label (v:vs) = do
--  let vm = varMap vm
  s <- get
  let vm = varMap s
  let vi = vm ! unFDTerm v
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
