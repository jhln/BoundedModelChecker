module NQueensSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
import NQueens
import Solver
import TreeSolver
import Transformers

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "solve (nrooksAssignments 0) == [[]]" $ do
      solve (nrooksAssignments 0) `shouldBe` [[]]
    it "solve (nrooksAssignments 1) == [[1]]" $ do
      solve (nrooksAssignments 1)  `shouldBe` [[1]]
    it "solve (nrooksAssignments 2) == [[1,2],[2,1]]" $ do
      solve (nrooksAssignments 2)  `shouldBe` [[1,2],[2,1]]
  describe "(run . ((flip evalt (DBST n))))" $ do
       modifyMaxSuccess (const 20) $ prop "should contain only solutions"
         depthBoundedProducesSubset


depthBoundedProducesSubset :: NonNegative Int -> NonNegative Int -> Bool
depthBoundedProducesSubset (NonNegative depth) (NonNegative rooks) = all (`elem` allsols) somesols
  where
    depth' = depth `rem` 10
    rooks' = rooks `rem` 9
    allsols = solve (nrooksAssignments rooks')
    somesols = (run . (`evalt` DBST depth')) (nrooksAssignments rooks')
