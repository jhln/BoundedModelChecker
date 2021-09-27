{-# LANGUAGE TypeFamilies #-}
module NQueens where



import Data.List(zip,
                 tails,
                )
import Data.Function(($))
import Prelude ()

import Language.CP.SearchTree
import Language.CP.FDSugar

nqueens n = exists (\queens -> model [queens] n)

model queens n = queens `allin` (1,n) /\
                 alldifferent queens /\
                 diagonals queens

allin queens range = conj [q `in_domain` range | q <- queens ]

alldifferent queens = conj [qi @\= qj | qi:qjs <- tails queens, qj <- qjs]

diagonals queens = conj [ qi @\== (qj @+ d) /\ qj @\== (qi @+ d)
                         | qi:qjs <- tails queens, (qj,d) <- zip qjs [1..]]
