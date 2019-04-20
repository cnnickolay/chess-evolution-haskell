module Chess.ChessSpec where

import Chess.Chess
import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad

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

  describe "rookMoves" $ do
    let
      location = coordinateToPosition D Four
      (Just board) = setCell emptyBoard (Cell White Pawn) (coordinateToPosition D Four)
    it "should move all directions" $ do
      let
        moves = Set.fromList $ rockMoves board location
        expectedMoves = Set.fromList [ (coordinateToPosition A Four)
                        , (coordinateToPosition B Four)
                        , (coordinateToPosition C Four)
                        , (coordinateToPosition E Four)
                        , (coordinateToPosition F Four)
                        , (coordinateToPosition G Four)
                        , (coordinateToPosition H Four)
                        , (coordinateToPosition D One)
                        , (coordinateToPosition D Two)
                        , (coordinateToPosition D Three)
                        , (coordinateToPosition D Five)
                        , (coordinateToPosition D Six)
                        , (coordinateToPosition D Seven)
                        , (coordinateToPosition D Eight)
                        ]
      moves `shouldBe` expectedMoves

    it "should attack up" $ do
      let
        (Just boardWithEnemy) = upperCell >>= lowerCell >>= rightCell >>= leftCell where
                                  upperCell = setCell board (Cell Black Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ rockMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [(coordinateToPosition D Five)]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location (coordinateToPosition D Five) `shouldBe` True

    it "should attack down" $ do
      let
        (Just boardWithEnemy) = upperCell >>= lowerCell >>= rightCell >>= leftCell where
                                  upperCell = setCell board (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell Black Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ rockMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [(coordinateToPosition D Three)]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location (coordinateToPosition D Three) `shouldBe` True

    it "should attack left" $ do
      let
        (Just boardWithEnemy) = upperCell >>= lowerCell >>= rightCell >>= leftCell where
                                  upperCell = setCell board (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell Black Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ rockMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [(coordinateToPosition C Four)]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location (coordinateToPosition C Four) `shouldBe` True

    it "should attack right" $ do
      let
        (Just boardWithEnemy) = upperCell >>= lowerCell >>= rightCell >>= leftCell where
                                  upperCell = setCell board (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell Black Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ rockMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [(coordinateToPosition E Four)]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location (coordinateToPosition E Four) `shouldBe` True

  describe "bishopMoves" $ do
    let
      location = coordinateToPosition D Four
      (Just board) = setCell emptyBoard (Cell White Pawn) (coordinateToPosition D Four)
    it "should move to all directions" $ do
      let
        moves = Set.fromList $ bishopMoves board location
        expectedMoves = Set.fromList [
                          (coordinateToPosition A One)
                        , (coordinateToPosition B Two)
                        , (coordinateToPosition C Three)
                        , (coordinateToPosition E Five)
                        , (coordinateToPosition F Six)
                        , (coordinateToPosition G Seven)
                        , (coordinateToPosition H Eight)
                        , (coordinateToPosition G One)
                        , (coordinateToPosition F Two)
                        , (coordinateToPosition E Three)
                        , (coordinateToPosition C Five)
                        , (coordinateToPosition B Six)
                        , (coordinateToPosition A Seven)
                        ]
      moves `shouldBe` expectedMoves

    it "should attack up and right" $ do
      let
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell where
                                  upperRightCell = setCell board (Cell Black Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
        moves = Set.fromList $ bishopMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [(coordinateToPosition E Five)]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location (coordinateToPosition E Five) `shouldBe` True

    it "should attack down and right" $ do
      let
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell Black Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
        moves = Set.fromList $ bishopMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [(coordinateToPosition E Three)]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location (coordinateToPosition E Three) `shouldBe` True

    it "should attack up and left" $ do
      let
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell Black Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
        moves = Set.fromList $ bishopMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [(coordinateToPosition C Five)]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location (coordinateToPosition C Five) `shouldBe` True

    it "should attack down and left" $ do
      let
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell Black Pawn) (coordinateToPosition C Three)
        moves = Set.fromList $ bishopMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [(coordinateToPosition C Three)]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location (coordinateToPosition C Three) `shouldBe` True

  describe "queenMoves" $ do
    let
      location = coordinateToPosition D Four
      (Just board) = setCell emptyBoard (Cell White Pawn) (coordinateToPosition D Four)
    it "should move to all directions" $ do
      let
        moves = Set.fromList $ queenMoves board location
        expectedMoves = Set.fromList [
                          (coordinateToPosition A One)
                        , (coordinateToPosition B Two)
                        , (coordinateToPosition C Three)
                        , (coordinateToPosition E Five)
                        , (coordinateToPosition F Six)
                        , (coordinateToPosition G Seven)
                        , (coordinateToPosition H Eight)
                        , (coordinateToPosition G One)
                        , (coordinateToPosition F Two)
                        , (coordinateToPosition E Three)
                        , (coordinateToPosition C Five)
                        , (coordinateToPosition B Six)
                        , (coordinateToPosition A Seven)
                        , (coordinateToPosition A Four)
                        , (coordinateToPosition B Four)
                        , (coordinateToPosition C Four)
                        , (coordinateToPosition E Four)
                        , (coordinateToPosition F Four)
                        , (coordinateToPosition G Four)
                        , (coordinateToPosition H Four)
                        , (coordinateToPosition D One)
                        , (coordinateToPosition D Two)
                        , (coordinateToPosition D Three)
                        , (coordinateToPosition D Five)
                        , (coordinateToPosition D Six)
                        , (coordinateToPosition D Seven)
                        , (coordinateToPosition D Eight)
                        ]
      moves `shouldBe` expectedMoves

    it "should attack up and right" $ do
      let
        attackCell = coordinateToPosition E Five
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell >>= upperCell >>= lowerCell >>= leftCell >>= rightCell where
                                  upperRightCell = setCell board (Cell Black Pawn) (attackCell)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
                                  upperCell b = setCell b (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ queenMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [attackCell]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location attackCell `shouldBe` True

    it "should attack down and right" $ do
      let
        attackCell = coordinateToPosition E Three
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell >>= upperCell >>= lowerCell >>= leftCell >>= rightCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell Black Pawn) attackCell
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
                                  upperCell b = setCell b (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ queenMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [attackCell]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location attackCell `shouldBe` True

    it "should attack up and left" $ do
      let
        attackCell = coordinateToPosition C Five
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell >>= upperCell >>= lowerCell >>= leftCell >>= rightCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell Black Pawn) attackCell
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
                                  upperCell b = setCell b (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ queenMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [attackCell]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location attackCell `shouldBe` True

    it "should attack down and left" $ do
      let
        attackCell = coordinateToPosition C Three
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell >>= upperCell >>= lowerCell >>= leftCell >>= rightCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell Black Pawn) attackCell
                                  upperCell b = setCell b (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ queenMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [attackCell]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location attackCell `shouldBe` True

    it "should attack up" $ do
      let
        attackCell = coordinateToPosition D Five
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell >>= upperCell >>= lowerCell >>= leftCell >>= rightCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
                                  upperCell b = setCell b (Cell Black Pawn) attackCell
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ queenMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [attackCell]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location attackCell `shouldBe` True

    it "should attack down" $ do
      let
        attackCell = coordinateToPosition D Three
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell >>= upperCell >>= lowerCell >>= leftCell >>= rightCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
                                  upperCell b = setCell b (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell Black Pawn) attackCell
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ queenMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [attackCell]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location attackCell `shouldBe` True

    it "should attack left" $ do
      let
        attackCell = coordinateToPosition C Four
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell >>= upperCell >>= lowerCell >>= leftCell >>= rightCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
                                  upperCell b = setCell b (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell Black Pawn) attackCell
                                  rightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Four)
        moves = Set.fromList $ queenMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [attackCell]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location attackCell `shouldBe` True

    it "should attack right" $ do
      let
        attackCell = coordinateToPosition E Four
        (Just boardWithEnemy) = upperRightCell >>= lowerRightCell >>= upperLeftCell >>= lowerLeftCell >>= upperCell >>= lowerCell >>= leftCell >>= rightCell where
                                  upperRightCell = setCell board (Cell White Pawn) (coordinateToPosition E Five)
                                  lowerRightCell b = setCell b (Cell White Pawn) (coordinateToPosition E Three)
                                  upperLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Five)
                                  lowerLeftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Three)
                                  upperCell b = setCell b (Cell White Pawn) (coordinateToPosition D Five)
                                  lowerCell b = setCell b (Cell White Pawn) (coordinateToPosition D Three)
                                  leftCell b = setCell b (Cell White Pawn) (coordinateToPosition C Four)
                                  rightCell b = setCell b (Cell Black Pawn) attackCell
        moves = Set.fromList $ queenMoves boardWithEnemy location
        expectedMoves = Set.fromList $ [attackCell]
      moves `shouldBe` expectedMoves
      isAttackMove boardWithEnemy location attackCell `shouldBe` True

  describe "knightMoves" $ do
    let f = knightMoves
    it "should move from the low left corner" $ do
      let
        locationH = A
        locationV = One
        setup = [(locationH, locationV, White, Knight)]
        expectedMoves = [(B, Three, False),
                         (C, Two, False)]
      checkMoves setup (locationH, locationV) f expectedMoves

    it "should move from the top left corner" $ do
      let
        locationH = A
        locationV = Eight
        setup = [(locationH, locationV, White, Knight)]
        expectedMoves = [(B, Six, False),
                         (C, Seven, False)]
      checkMoves setup (locationH, locationV) f expectedMoves

    it "should move from the top right corner" $ do
      let
        locationH = H
        locationV = Eight
        setup = [(locationH, locationV, White, Knight)]
        expectedMoves = [(G, Six, False),
                         (F, Seven, False)]
      checkMoves setup (locationH, locationV) f expectedMoves

    it "should move from the low right corner" $ do
      let
        locationH = H
        locationV = One
        setup = [(locationH, locationV, White, Knight)]
        expectedMoves = [(G, Three, False),
                         (F, Two, False)]
      checkMoves setup (locationH, locationV) f expectedMoves

    it "should move from the center" $ do
      let
        locationH = D
        locationV = Four
        setup = [(locationH, locationV, White, Knight)]
        expectedMoves = [(E, Six, False),
                         (F, Five, False),
                         (F, Three, False),
                         (E, Two, False),
                         (C, Two, False),
                         (B, Three, False),
                         (B, Five, False),
                         (C, Six, False)
                        ]
      checkMoves setup (locationH, locationV) f expectedMoves

    it "should attack" $ do
      let
        locationH = D
        locationV = Four
        setup = [ (locationH, locationV, White, Knight)
                , (E, Six, Black, Pawn)
                , (F, Five, Black, Pawn)
                , (E, Two, Black, Pawn)
                ]
        expectedMoves = [(E, Six, True),
                         (F, Five, True),
                         (F, Three, False),
                         (E, Two, True),
                         (C, Two, False),
                         (B, Three, False),
                         (B, Five, False),
                         (C, Six, False)
                        ]
      checkMoves setup (locationH, locationV) f expectedMoves

checkMoves :: [(HorizontalAxis, VerticalAxis, Color, Figure)] -> (HorizontalAxis, VerticalAxis) -> (Board -> Position -> [Position]) -> [(HorizontalAxis, VerticalAxis, Bool)] -> IO ()
checkMoves setup initLocation@(iH, iV) f expectation =
  let
    normalize :: [Position] -> [(HorizontalAxis, VerticalAxis, Bool)]
    normalize (p:ps) = normalizeOne p ++ normalize ps where
                        normalizeOne p = maybeToList $ fmap coordinatesToMove $ positionToCoordinate p
                        coordinatesToMove = (\(h, v) -> (h, v, (isAttackMove board (coordinateToPosition iH iV) (coordinateToPosition h v))) )
    normalize _ = []

    from = coordinateToPosition iH iV
    (Just board) = foldM (\b (h, v, c, f) -> setCell b (Cell c f) (coordinateToPosition h v)) emptyBoard setup
    result = Set.fromList (normalize $ f board from)
  in
    result `shouldBe` (Set.fromList expectation)