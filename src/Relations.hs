module Relations where

import Sets
import Prelude hiding (curry, uncurry, flip, elem)

type Rel a = Set (a,a)

-- domR gives the domain of a relation.
domR :: Ord a => Rel a -> Set a
domR (Set r) = list2set [ x | (x,_) <- r ]

-- ranR gives the range of a relation.
ranR :: Ord a => Rel a -> Set a
ranR (Set r) = list2set [ y | (_,y) <- r ]

-- idR creates the identity relation ∆A over a set A:
idR :: Ord a => Set a -> Rel a
idR (Set xs) = Set [(x,x) | x <- xs]

-- The total relation over a set is given by:
totalR :: Set a -> Rel a
totalR (Set xs) = Set [(x,y) | x <- xs, y <- xs ]

-- invR inverts a relation (i.e., the function maps R to R−1
invR :: Ord a => Rel a -> Rel a
invR (Set []) = (Set [])
invR (Set ((x,y):r)) = insertSet (y,x) (invR (Set r))

-- inR checks whether a pair is in a relation.
inR :: Ord a => Rel a -> (a,a) -> Bool
inR r (x,y) = inSet (x,y) r

-- complement of a relation R
complR :: Ord a => Set a -> Rel a -> Rel a
complR (Set xs) r =
  Set [ (x,y) | x <- xs
              , y <- xs
              , not (inR r (x,y))]

-- A check for reflexivity of R
reflR :: Ord a => Set a -> Rel a -> Bool
reflR set r = subSet (idR set) r

-- A check for irreflexivity of R on A p
irreflR :: Ord a => Set a -> Rel a -> Bool
irreflR (Set xs) r =
  all (\ pair -> not (inR r pair)) [(x,x) | x <- xs]

-- A check for symmetry of R
symR :: Ord a => Rel a -> Bool
symR (Set []) = True
symR (Set ((x,y):pairs)) 
  | x == y = symR (Set pairs)
  | otherwise = 
        inSet (y,x) (Set pairs)
        && symR (deleteSet (y,x) (Set pairs))

-- A check for transitivity of R
transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s) = and [ trans pair (Set s) | pair <- s ] 
  where
    trans (x,y) (Set r) =
      and [ inSet (x,v) (Set r) | (u,v) <- r, u == y ]

composePair :: Ord a => (a,a) -> Rel a -> Rel a
composePair (x,y) (Set []) = Set []
composePair (x,y) (Set ((u,v):s))
  | y == u = insertSet (x,v) (composePair (x,y) (Set s))
  | otherwise = composePair (x,y) (Set s)

unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (Set []) set2 = set2
unionSet (Set (x:xs)) set2 =
  insertSet x (unionSet (Set xs) (deleteSet x set2))

compR :: Ord a => Rel a -> Rel a -> Rel a
compR (Set []) _ = (Set [])
compR (Set ((x,y):s)) r =
  unionSet (composePair (x,y) r) (compR (Set s) r)

repeatR :: Ord a => Rel a -> Int -> Rel a
repeatR r n 
  | n < 1 = error "argument < 1"
  | n == 1 = r
  | otherwise = compR r (repeatR r (n-1))

r = Set [(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)]
r2 = compR r r
r3 = repeatR r 3
r4 = repeatR r 4

s = Set [(0,0),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,2),(2,3)]
test = (True ==) $ (unionSet r (compR s r)) == s 

rclosR :: Ord a => Rel a -> Rel a
rclosR r = unionSet r (idR background)
  where background = unionSet (domR r) (ranR r)

sclosR :: Ord a => Rel a -> Rel a
sclosR r = unionSet r (invR r)

tclosR :: Ord a => Rel a -> Rel a
tclosR r 
  | transR r = r
  | otherwise = tclosR (unionSet r (compR r r))


-------------- 
--- Implementing Relations as Characteristic Functions

divides :: Integer -> Integer -> Bool
divides d n 
  | d == 0 = error "divides: zero divisor"
  | otherwise = (rem n d) == 0

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f p = f (fst p) (snd p)

eq :: Eq a => (a,a) -> Bool
eq = uncurry (==)
lessEq :: Ord a => (a,a) -> Bool
lessEq = uncurry (<=)

inverse :: ((a,b) -> c) -> ((b,a) -> c)
inverse f (x,y) = f (y,x)

invTest1 = inverse lessEq (3,4)
invTest2 = inverse lessEq (4,3)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

devidesTest = filter (flip divides $ 102) [1..300]




-----------------------
--- Second def

type Rel' a = a -> a -> Bool

emptyR' :: Rel' a
emptyR' = \ _ _ -> False

list2rel' :: Eq a => [(a,a)] -> Rel' a
list2rel' xys = \ x y -> elem (x,y) xys

idR' :: Eq a => [a] -> Rel' a
idR' xs = \ x y -> x == y && elem x xs

invR' :: Rel' a -> Rel' a
invR' = flip

inR' :: Rel' a -> (a,a) -> Bool
inR' = uncurry

reflR' :: [a] -> Rel' a -> Bool
reflR' xs r = and [ r x x | x <- xs ]

irreflR' :: [a] -> Rel' a -> Bool
irreflR' xs r = and [ not (r x x) | x <- xs ]

symR' :: [a] -> Rel' a -> Bool
symR' xs r = and [ not (r x y && not (r y x)) | x <- xs, y <- xs ]

transR' :: [a] -> Rel' a -> Bool
transR' xs r = 
  and 
  [ not (r x y && r y z && not (r x z))
    | x <- xs, y <- xs, z <- xs ]

unionR' :: Rel' a -> Rel' a -> Rel' a
unionR' r s x y = r x y || s x y

intersR' :: Rel' a -> Rel' a -> Rel' a
intersR' r s x y = r x y && s x y

reflClosure' :: Eq a => Rel' a -> Rel' a
reflClosure' r = unionR' r (==)

symClosure' :: Rel' a -> Rel' a
symClosure' r = unionR' r (invR' r)

compR' :: [a] -> Rel' a -> Rel' a -> Rel' a
compR' xs r s x y = or [ r x z && s z y | z <- xs ]

repeatR' :: [a] -> Rel' a -> Int -> Rel' a
repeatR' xs r n | n < 1 = error "argument < 1"
  | n == 1 = r
  | otherwise = compR' xs r (repeatR' xs r (n-1))

inDegree :: (Eq a) => Rel a -> a -> Int
inDegree (Set r) = \ x -> length [ y | (_,y) <- r, y == x ]
outDegree :: (Eq a) => Rel a -> a -> Int
outDegree (Set r) = \ x -> length [ y | (y,_) <- r, y == x ]
{-
sources :: (Eq a) => Rel a -> Set a
sources (Set r) = 
  Set [ x | x <- union (map fst r) (map snd r) 
          , inDegree (Set r) x == 0 
          , outDegree (Set r) x >= 1 ]

sinks :: (Eq a) => Rel a -> Set a
sinks (Set r) = 
  Set [ x | x <- union (map fst r) (map snd r)
          , outDegree (Set r) x == 0
          , inDegree (Set r) x >= 1 ]
-}

successor :: Rel' Int
successor = \ n m -> n+1 == m
rel = unionR' successor (repeatR' [0..1000] successor 2)


--- Equivalenz Relations

equivalenceR :: Ord a => Set a -> Rel a -> Bool
equivalenceR set r = reflR set r && symR r && transR r

equivalenceR' :: [a] -> Rel' a -> Bool
equivalenceR' xs r = reflR' xs r && symR' xs r && transR' xs r

modulo :: Integer -> Integer -> Integer -> Bool
modulo n = \ x y -> divides n (x-y)

equalSize :: [a] -> [b] -> Bool
equalSize list1 list2 = (length list1) == (length list2)


----- Equivalence Classes and Partitions

rclass :: Rel' a -> a -> [a] -> [a]
rclass r x ys = [ y | y <- ys, r x y ]


----- Integer Partitions

type Part = [Int]
type CmprPart = (Int,Part)

expand :: CmprPart -> Part
expand (0,p) = p
expand (n,p) = 1:(expand ((n-1),p))

nextpartition :: CmprPart -> CmprPart
nextpartition (k,(x:xs)) = pack (x-1) ((k+x),xs)

pack :: Int -> CmprPart -> CmprPart
pack 1 (m,xs) = (m,xs)
pack k (m,xs) = 
  if k > m 
    then pack (k-1) (m,xs)
    else pack k (m-k,k:xs)
  
generatePs :: CmprPart -> [Part]
generatePs p@(n,[]) = [expand p]
generatePs p@(n,(x:xs)) = 
  (expand p : generatePs(nextpartition p))

part :: Int -> [Part]
part n 
  | n < 1 = error "part: argument <= 0"
  | n == 1 = [[1]]
  | otherwise = generatePs (0,[n])

bell :: Integer -> Integer
bell 0 = 1
bell n = sum [stirling n k | k <- [1..n]]

stirling :: Integer -> Integer -> Integer
stirling n 1 = 1
stirling n k 
  | n == k = 1
  | otherwise = k * (stirling (n-1) k) + stirling (n-1) (k-1)

listPartition :: Eq a => [a] -> [[a]] -> Bool
listPartition xs xss =
  all (flip elem $ xs) (concat xss) 
  && all (flip elem $ (concat xss)) xs
  && listPartition' xss []
    where
      listPartition' [] _ = True
      listPartition' ([]:xss) _ = False
      listPartition' (xs:xss) domain
        | intersect xs domain == [] = 
            listPartition' xss (union xs domain)
        | otherwise = False


