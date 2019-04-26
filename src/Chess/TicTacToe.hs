module Chess.TicTacToe where

import Control.Monad
import Control.Monad.Trans.State
import Data.Array
import Data.List.Split
import Data.List
import Chess.NeuralNetwork

type Coordinate = (Int, Int)
data Player = Cross | Zero deriving (Show, Eq)
data CellType = Empty | Cell Player deriving (Show, Eq)

type Board = Array Coordinate CellType

newBoard :: Board
newBoard = array ((1, 1), (3, 3)) [ ((1, 1), Empty)
                                  , ((1, 2), Empty)
                                  , ((1, 3), Empty)
                                  , ((2, 1), Empty)
                                  , ((2, 2), Empty)
                                  , ((2, 3), Empty)
                                  , ((3, 1), Empty)
                                  , ((3, 2), Empty)
                                  , ((3, 3), Empty)
                                  ]

newGame :: State Board ()
newGame = put newBoard

setCell :: CellType -> (Int, Int) -> State Board ()
setCell cell coord = do
                    s <- get
                    let a = s // [(coord, cell)]
                    put a
                    pure ()

findWinner :: State Board (Maybe Player)
findWinner = do
               game <- get
               let
                   lines = chunksOf 3 (   [ (k, n) | k <- [1..3], n <- [1..3] ]
                                       ++ [ (n, k) | k <- [1..3], n <- [1..3] ]
                                       ++ (zip [1..3] [1..3])
                                       ++ (zip [1..3] $ reverse [1..3])
                                       )
                   getWinner :: [Coordinate] -> CellType
                   getWinner (c:cs) = foldl (\cell c -> if cell == game ! c then cell else Empty) (game ! c) cs
                   winner :: Maybe Player
                   winner = fmap (\(Cell a) -> a) $ find (\a -> a /= Empty) $ map getWinner lines
               pure winner

allMoves :: State Board [Coordinate]
allMoves =
  do
    board <- get
    let emptyCells = filter isEmpty $ assocs board where
                                      isEmpty (i, Empty) = True
                                      isEmpty _ = False
        cells = (\(a, _) -> a) <$> emptyCells
    pure cells

--boardPermutations :: Player -> State Board [Board]
--boardPermutations player =
--  do
--    board <- get
--    moves <- allMoves player
--    let boards = generateBoard <$> moves where
--                              generateBoard c = execState (setCell (Cell player) c) board
--    pure boards

estimateBoard :: NN -> Player -> Board -> Float
estimateBoard nn player board =
  let
    cellToNumber :: CellType -> Float
    cellToNumber Empty = 0.0
    cellToNumber (Cell Cross) = 1.0
    cellToNumber (Cell Zero) = 2.0
    input = cellToNumber <$> elems board
    (result:_) = execute nn input
  in
    result

nextMove :: NN -> Player -> State Board (Maybe Coordinate)
nextMove nn player =
  do
    board <- get
    moves <- allMoves
    let
      generateBoard c = execState (setCell (Cell player) c) board
      boards = generateBoard <$> moves
      estimated = zip (estimateBoard nn player <$> boards) moves
      sorted = sortBy ordering estimated
      ordering (a, _) (b, _)
        | a > b     = GT
        | a < b     = LT
        | otherwise = EQ
      head (x:xs) = Just x
      head _ = Nothing
    pure $ fmap (\(_, coord) -> coord) $ head sorted

playGame :: NN -> NN -> ((Maybe Player), Board)
playGame nn1 nn2 =
  let
    winner = play' nn1 Zero where
               play' :: NN -> Player -> State Board (Maybe Player)
               play' nn player = do
                                   coordinateM <- nextMove nn player
                                   case coordinateM of
                                     Nothing         -> pure Nothing
                                     Just coordinate -> do
                                                          setCell (Cell player) coordinate
                                                          winner <- findWinner
                                                          case winner of
                                                            Just p  -> pure winner
                                                            Nothing -> if player == Zero then play' nn2 Cross
                                                                       else play' nn1 Zero
    result = runState winner newBoard
  in result

play :: IO ()
play = do
         let
           wNNOrig = network [9, 4, 1]
           bNNOrig = network [9, 4, 1]
         wNN <- mutateNetwork 0.1 wNNOrig
         bNN <- mutateNetwork 0.1 bNNOrig
         let (winner, board) = playGame wNN bNN
         printBoard board
         putStrLn $ show $ winner
         pure ()

cellToText (Cell Zero) = "0"
cellToText (Cell Cross) = "X"
cellToText (Empty) = "_"

printBoard :: Board -> IO ()
printBoard b = do
                 putStr   $ cellToText $ b ! (1,1)
                 putStr   $ cellToText $ b ! (1,2)
                 putStrLn $ cellToText $ b ! (1,3)
                 putStr   $ cellToText $ b ! (2,1)
                 putStr   $ cellToText $ b ! (2,2)
                 putStrLn $ cellToText $ b ! (2,3)
                 putStr   $ cellToText $ b ! (3,1)
                 putStr   $ cellToText $ b ! (3,2)
                 putStrLn $ cellToText $ b ! (3,3)
