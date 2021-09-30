{-# LANGUAGE TypeFamilies #-}
module Simple where

import FD
import Tree
import TreeSugar

test :: Tree FD ()
test = NewVar $ \v -> (Add (Same (Var v) (Const 1))) $ Return ()

test2 :: Tree FD ()
test2 = NewVar $ \v -> (Try
                        (Add (Same (Var v) (Const 1)) (Return ()))
                        (Add (Same (Var v) (Const 2)) (Return ())))
test3 :: Tree FD ()
test3 = exist 3 (\vs -> model vs)

model vars = conj [ ((Var v) @= 1) \/ ((Var v) @= 2) | v <- vars ]

--test = do
    --x <- newVar [0..3]
    --y <- newVar [0..3]
    --(( x .<. y) \/ (x `same` y))
    --x `hasValue` 2
