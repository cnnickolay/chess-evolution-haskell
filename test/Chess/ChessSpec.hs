module Chess.ChessSpec where

import Chess.Chess
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
  describe "positionToCoordinate" $ do
    it "should convert integer to coordinate" $ do
      positionToCoordinate 10 `shouldBe` Just (B, Two)
      positionToCoordinate 64 `shouldBe` Just (H, Eight)
      positionToCoordinate 1 `shouldBe` Just (A, One)
      positionToCoordinate 65 `shouldBe` Nothing
      positionToCoordinate 0 `shouldBe` Nothing

  describe "coordinateToPosition" $ do
    it "should convert coordinate to integer" $ do
      coordinateToPosition A One `shouldBe` 1
      coordinateToPosition H Eight `shouldBe` 64
      coordinateToPosition E Four `shouldBe` 29

  describe "left" $ do
    it "should shift horizontal coordinate to one cell left" $ do
      left A `shouldBe` Nothing
      left B `shouldBe` Just A

  describe "right" $ do
    it "right should shift horizontal coordinate to one cell right" $ do
      right A `shouldBe` Just B
      right H `shouldBe` Nothing

  describe "up" $ do
    it "up should shift vertical coordinate to one cell up" $ do
      up One `shouldBe` Just Two
      up Eight `shouldBe` Nothing

  describe "down" $ do
    it "down should shift vertical coordinate to one cell down" $ do
      down One `shouldBe` Nothing
      down Eight `shouldBe` Just Seven

  describe "setCell" $ do
    it "should update any cell on the board" $ do
      let board = emptyBoard
          position = 10
      cellAt board position `shouldBe` Just Empty
      let (Just boardWithFigure) = setCell board (Cell White Bishop) position
      cellAt boardWithFigure position `shouldBe` Just (Cell White Bishop)
      let (Just rolledBackFigure) = setCell board Empty position
      cellAt rolledBackFigure position `shouldBe` Just Empty

    it "should return Nothing if bad position given" $ do
      cellAt newBoard 0 `shouldBe` Nothing
      cellAt newBoard 65 `shouldBe` Nothing

  describe "pawnMoves for whites" $ do
    it "should return one move from row One" $ do
      let (Just board) = setCell emptyBoard (Cell White Pawn) (coordinateToPosition A One)
          moves = pawnMoves board (coordinateToPosition A One)
      moves `shouldBe` [(coordinateToPosition A Two)]

    it "should return two moves from row Two" $ do
      let (Just board) = setCell emptyBoard (Cell White Pawn) (coordinateToPosition A Two)
          moves = pawnMoves board (coordinateToPosition A Two)
      moves `shouldBe` [(coordinateToPosition A Three), (coordinateToPosition A Four)]

    it "should return zero moves from row Eight" $ do
      let (Just board) = setCell emptyBoard (Cell White Pawn) (coordinateToPosition A Eight)
          moves = pawnMoves board (coordinateToPosition A Eight)
      moves `shouldBe` []

    it "should return zero moves if cell is blocked" $ do
      let (Just board) = do
                           board' <- setCell emptyBoard (Cell White Pawn) (coordinateToPosition A One)
                           setCell board' (Cell White Pawn) (coordinateToPosition A Two)
          moves = pawnMoves board (coordinateToPosition A One)
      moves `shouldBe` []

    it "should return zero moves if cell is blocked and it's on row Two" $ do
      let (Just board) = do
                           board' <- setCell emptyBoard (Cell White Pawn) (coordinateToPosition A Two)
                           setCell board' (Cell White Pawn) (coordinateToPosition A Three)
          moves = pawnMoves board (coordinateToPosition A Two)
      moves `shouldBe` []

    it "should return only one move if +2 cell is blocked and it's on row Two" $ do
      let (Just board) = do
                           board' <- setCell emptyBoard (Cell White Pawn) (coordinateToPosition A Two)
                           setCell board' (Cell White Pawn) (coordinateToPosition A Four)
          moves = pawnMoves board (coordinateToPosition A Two)
      moves `shouldBe` [(coordinateToPosition A Three)]

    it "should attack to up and right" $ do
      let (Just board) = whitePawn >>= blockingWhitePawn >>= blackPawnUnderAttack where
                          whitePawn                 = setCell emptyBoard (Cell White Pawn) (coordinateToPosition B Two)
                          blockingWhitePawn b       = setCell b (Cell White Pawn) (coordinateToPosition B Three)
                          blackPawnUnderAttack b    = setCell b (Cell Black Pawn) (coordinateToPosition C Three)
      let moves = pawnMoves board (coordinateToPosition B Two)
      moves `shouldBe` [(coordinateToPosition C Three)]
      isAttackMove board (coordinateToPosition B Two) (coordinateToPosition C Three) `shouldBe` True

    it "should attack to up and left" $ do
      let (Just board) = whitePawn >>= blockingWhitePawn >>= blackPawnUnderAttack where
                          whitePawn                 = setCell emptyBoard (Cell White Pawn) (coordinateToPosition B Two)
                          blockingWhitePawn b       = setCell b (Cell White Pawn) (coordinateToPosition B Three)
                          blackPawnUnderAttack b    = setCell b (Cell Black Pawn) (coordinateToPosition A Three)
      let moves = pawnMoves board (coordinateToPosition B Two)
      moves `shouldBe` [(coordinateToPosition A Three)]
      isAttackMove board (coordinateToPosition B Two) (coordinateToPosition A Three) `shouldBe` True

  describe "pawnMoves for blacks" $ do
    it "should return one move from row Eight" $ do
      let (Just board) = setCell emptyBoard (Cell Black Pawn) (coordinateToPosition A Eight)
          moves = pawnMoves board (coordinateToPosition A Eight)
      moves `shouldBe` [(coordinateToPosition A Seven)]

    it "should return two moves from row Seven" $ do
      let (Just board) = setCell emptyBoard (Cell Black Pawn) (coordinateToPosition A Seven)
          moves = pawnMoves board (coordinateToPosition A Seven)
      moves `shouldBe` [(coordinateToPosition A Six), (coordinateToPosition A Five)]

    it "should return zero moves from row One" $ do
      let (Just board) = setCell emptyBoard (Cell Black Pawn) (coordinateToPosition A One)
          moves = pawnMoves board (coordinateToPosition A One)
      moves `shouldBe` []

    it "should return zero moves if cell is blocked" $ do
      let (Just board) = do
                           board' <- setCell emptyBoard (Cell Black Pawn) (coordinateToPosition A Eight)
                           setCell board' (Cell Black Pawn) (coordinateToPosition A Seven)
          moves = pawnMoves board (coordinateToPosition A Eight)
      moves `shouldBe` []

    it "should return zero moves if cell is blocked and it's on row Two" $ do
      let (Just board) = do
                           board' <- setCell emptyBoard (Cell Black Pawn) (coordinateToPosition A Seven)
                           setCell board' (Cell Black Pawn) (coordinateToPosition A Six)
          moves = pawnMoves board (coordinateToPosition A Seven)
      moves `shouldBe` []

    it "should return only one move if +2 cell is blocked and it's on row Seven" $ do
      let (Just board) = do
                           board' <- setCell emptyBoard (Cell Black Pawn) (coordinateToPosition A Seven)
                           setCell board' (Cell Black Pawn) (coordinateToPosition A Five)
          moves = pawnMoves board (coordinateToPosition A Seven)
      moves `shouldBe` [(coordinateToPosition A Six)]

    it "should attack to up and right" $ do
      let (Just board) = blackPawn >>= blockingBlackPawn >>= whitePawnUnderAttack where
                          blackPawn                 = setCell emptyBoard (Cell Black Pawn) (coordinateToPosition B Seven)
                          blockingBlackPawn b       = setCell b (Cell Black Pawn) (coordinateToPosition B Six)
                          whitePawnUnderAttack b    = setCell b (Cell White Pawn) (coordinateToPosition C Six)
      let moves = pawnMoves board (coordinateToPosition B Seven)
      moves `shouldBe` [(coordinateToPosition C Six)]
      isAttackMove board (coordinateToPosition B Seven) (coordinateToPosition C Six) `shouldBe` True

    it "should attack to up and left" $ do
      let (Just board) = blackPawn >>= blockingBlackPawn >>= whitePawnUnderAttack where
                          blackPawn                 = setCell emptyBoard (Cell Black Pawn) (coordinateToPosition B Seven)
                          blockingBlackPawn b       = setCell b (Cell Black Pawn) (coordinateToPosition B Six)
                          whitePawnUnderAttack b    = setCell b (Cell White Pawn) (coordinateToPosition A Six)
      let moves = pawnMoves board (coordinateToPosition B Seven)
      moves `shouldBe` [(coordinateToPosition A Six)]
      isAttackMove board (coordinateToPosition B Seven) (coordinateToPosition A Six) `shouldBe` True
