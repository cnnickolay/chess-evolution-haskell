module Chess.TicTacToeSpec where

import Chess.TicTacToe
import Test.Hspec
import Test.QuickCheck
import Data.Array
import Control.Monad.Trans.State

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
  describe "new board" $ do
    it "test" $ do
      newBoard ! (1, 1) `shouldBe` Empty

  describe "findWinner" $ do
    it "no winner should be found on new board" $ do
      (evalState findWinner newBoard) `shouldBe` Nothing

    it "vertical winner" $ do
      let f = (do
                  setCell (Cell Zero) (1, 1) >> setCell (Cell Zero) (1, 2) >> setCell (Cell Zero) (1, 3)
                  findWinner
              )

      (evalState f newBoard) `shouldBe` Just Zero

    it "horizontal winner" $ do
      let f = (do
                  setCell (Cell Cross) (1, 2) >> setCell (Cell Cross) (2, 2) >> setCell (Cell Cross) (3, 2)
                  findWinner
              )

      (evalState f newBoard) `shouldBe` Just Cross

    it "diagonal winner" $ do
      let f = (do
                  setCell (Cell Cross) (1, 1) >> setCell (Cell Cross) (2, 2) >> setCell (Cell Cross) (3, 3)
                  findWinner
              )

      (evalState f newBoard) `shouldBe` Just Cross

    it "backward diagonal winner" $ do
      let f = (do
                  setCell (Cell Zero) (3, 1) >> setCell (Cell Zero) (2, 2) >> setCell (Cell Zero) (1, 3)
                  findWinner
              )

      (evalState f newBoard) `shouldBe` Just Zero

  describe "allMoves" $ do
    it "return all cells on empty board" $ do
      (evalState (allMoves Cross) newBoard) `shouldBe` [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

    it "returns only empty cells" $ do
      let f = (do
                  setCell (Cell Zero) (3, 1) >> setCell (Cell Zero) (2, 2) >> setCell (Cell Zero) (1, 3)
                  allMoves Zero
              )

      (evalState f newBoard) `shouldBe`  [(1,1),(1,2),(2,1),(2,3),(3,2),(3,3)]