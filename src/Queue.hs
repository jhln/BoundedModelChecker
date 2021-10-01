{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Queue where

import qualified Data.Sequence
--import qualified Language.CP.PriorityQueue as PriorityQueu

class Queue q where
  type Elem q :: *
  emptyQ :: q -> q
  isEmptyQ :: q -> Bool
  popQ :: q -> (Elem q, q)
  pushQ :: Elem q -> q -> q



instance Queue [a] where
  type Elem [a] = a
  emptyQ _     = []
  isEmptyQ     = Prelude.null
  popQ (x:xs)  = (x,xs)
  pushQ        = (:)

instance Queue (Data.Sequence.Seq a) where
  type Elem (Data.Sequence.Seq a)  = a
  emptyQ _                   = Data.Sequence.empty
  isEmptyQ                   = Data.Sequence.null
  popQ (Data.Sequence.viewl -> x Data.Sequence.:< xs)  = (x,xs)
  pushQ                      = flip (Data.Sequence.|>)
