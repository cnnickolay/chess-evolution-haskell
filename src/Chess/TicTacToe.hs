module Chess.TicTacToe where

import Control.Monad
import Control.Monad.Trans.State
import Data.Array
import Data.List.Split
import Data.List
import Chess.NeuralNetwork
import Data.Char

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

setCellOnBoard :: Board -> CellType -> (Int, Int) -> Board
setCellOnBoard board cell coord = execState (setCell cell coord) board

setCell :: CellType -> (Int, Int) -> State Board ()
setCell cell coord = do
                    s <- get
                    let a = s // [(coord, cell)]
                    put a
                    pure ()

findWinnerOnBoard :: Board -> (Maybe Player)
findWinnerOnBoard board = evalState (findWinner) board

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

allMovesOnBoard :: Board -> [Coordinate]
allMovesOnBoard board = evalState (allMoves) board

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
    cellToNumber Empty = 1.0
    cellToNumber (Cell Cross) = 2.0
    cellToNumber (Cell Zero) = 3.0
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
playGame zeroNN crossNN =
  let
    winner = play' zeroNN Zero where
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
                                                            Nothing -> if player == Zero then play' crossNN Cross
                                                                       else play' zeroNN Zero
    result = runState winner newBoard
  in result

evolveUntilWin :: NN -> NN -> Player -> Int -> IO (Maybe NN)
evolveUntilWin zeroNN crossNN player max = evolveUntilWin' 0 where
  evolveUntilWin' :: Int -> IO (Maybe NN)
  evolveUntilWin' current = do
                              nns <- breedNN 0.5 50 (if player == Cross then crossNN else zeroNN)
                              let
                                playGame' nn
                                  | player == Zero = playGame nn crossNN
                                  | otherwise      = playGame zeroNN nn
                                filterPlayer (nn, ((Just p), _))
                                  | p == player = True
                                filterPlayer _  = False
                                extract (nn, ((Just p), _)) = nn
                                winners = map extract $
                                          filter filterPlayer $
                                          zip nns (playGame' <$> nns)
                                result = case winners of
                                  [] | current < max -> evolveUntilWin' (current+1)
                                     | otherwise     -> pure Nothing
                                  _                  -> pure $ Just $ squashNNs winners
                              result

evolve :: Int -> NN -> NN -> IO (NN, NN)
evolve steps originalZeroNN originalCrossNN =
  evolve' originalZeroNN originalCrossNN 0 Zero where
    evolve' :: NN -> NN -> Int -> Player -> IO (NN, NN)
    evolve' zeroNN crossNN currentStep player = do
                                                  putStrLn $ "Step " ++ (show currentStep)
                                                  evolvedNN_M <- evolveUntilWin zeroNN crossNN player 200
                                                  case evolvedNN_M of
                                                    (Just evolvedNN) | currentStep < steps && player == Zero  -> evolve' evolvedNN crossNN (currentStep+1) Cross
                                                    (Just evolvedNN) | currentStep < steps && player == Cross -> evolve' zeroNN evolvedNN (currentStep+1) Zero
                                                    (Just evolvedNN) | player == Zero -> pure $ (evolvedNN, crossNN)
                                                    (Just evolvedNN) | player == Cross -> pure $ (zeroNN, evolvedNN)
                                                    _ -> pure (zeroNN, crossNN)

invertPlayer Zero = Cross
invertPlayer Cross = Zero

playAgainstNN :: NN -> Player -> IO ()
playAgainstNN nn humanPlayer =
  putStrLn (show nn) >> playAgainstNN' newBoard humanPlayer where
              oneMoreGame :: IO ()
              oneMoreGame = do
                              putStrLn "One more game? "
                              yn <- getChar
                              if yn == 'y' then playAgainstNN' newBoard humanPlayer
                              else pure ()

              playAgainstNN' :: Board -> Player -> IO ()
              playAgainstNN' board currentPlayer =
                do
                  putStrLn ""
                  printBoard board
                  boardM <-
                            if humanPlayer == currentPlayer then
                              do
                                let
                                  board' (a:b:_)
                                    | isDigit a && isDigit b = Just $ setCellOnBoard board (Cell humanPlayer) (digitToInt a, digitToInt b)
                                  board' _ = Nothing

                                putStrLn "Make your move human (IntInt, e.g. 31): "
                                fmap board' getLine
                            else
                              pure $ evalState (do
                                                  computerCoords <- nextMove nn currentPlayer
                                                  case computerCoords of
                                                    Just (coords) -> pure $ Just $ setCellOnBoard board (Cell currentPlayer) coords
                                                    Nothing       -> pure Nothing
                                               ) board
                  case boardM of
                    (Just board') ->
                        let
                          winner' = findWinnerOnBoard board'
                        in case winner' of
                          (Just winner) | winner == humanPlayer -> printBoard board' >> putStrLn "Human won" >> oneMoreGame
                                        | otherwise             -> printBoard board' >> putStrLn "Computer won" >> oneMoreGame
                          Nothing | allMovesOnBoard board' == [] -> putStrLn "Draw" >> oneMoreGame
                                  | otherwise                    -> playAgainstNN' board' $ invertPlayer currentPlayer
                    Nothing -> putStrLn "Pull yourself together pathetic human. One more try" >> playAgainstNN' board currentPlayer

play :: IO ()
play = do
         let
           zeroOrig = network [9, 9, 1]
           crossOrig = network [9, 9, 1]
         (zeroNN, crossNN) <- evolve 1000 zeroOrig crossOrig
         let (winner, board) = playGame zeroNN crossNN
         printBoard board
         putStrLn $ show $ winner
         playAgainstNN crossNN Zero
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
