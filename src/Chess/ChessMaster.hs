module Chess.ChessMaster where

import Chess.NeuralNetwork
import Chess.Chess
import Control.Monad
import Control.Monad.Trans.State
import Data.Array
import Numeric.Extra
import Data.Maybe

data Game = Game Board [Move] (Maybe Color) deriving (Show, Eq)
type GameState = State Game ()

data Move = Move {
              from :: (HorizontalAxis, VerticalAxis),
              to :: (HorizontalAxis, VerticalAxis)
            } deriving (Show, Eq)

newGame :: Game
newGame = Game newBoard [] Nothing

play :: NN -> NN -> Int -> GameState
play wNN bNN moves = foldM play' () (concat $ replicate moves [White, Black]) where
                  play' :: () -> Color -> GameState
                  play' _ White = chooseBestMove White wNN
                  play' _ Black = chooseBestMove Black bNN

chooseBestMove :: Color -> NN -> GameState
chooseBestMove c nn = do
                        origState @ (Game board moves colorWon) <- get
                        let m = boardPermutations c board
                            boardsWithProbabilities = (\(b, from, to) -> (winProbability nn b, b, from, to)) <$> m
                            findBestBoard :: [(Float, Board, Position, Position)] -> (Float, Board, Position, Position)
                            findBestBoard (x:[]) = x
                            findBestBoard (x:xs) = foldl (\aa@(a, _, _, _) bb@(b, _, _, _) -> if a > b then aa else bb) x xs
                            (_, bestBoard, from, to) = findBestBoard boardsWithProbabilities
                            move = maybeToList $ (\a b -> Move {from = a, to = b}) <$> (positionToCoordinate from) <*> (positionToCoordinate to)
                        case colorWon of
                          Just _ -> put origState
                          _ -> if length m > 0 then put $ Game bestBoard (moves ++ move) Nothing else put $ Game bestBoard (moves ++ move) (Just $ inverseColor c)

winProbability :: NN -> Board -> Float
winProbability nn b =
  let
    input = map cellToNumber $ (assocs b) where
                cellToNumber :: (Int, Cell) -> Float
                cellToNumber (idx, Empty) = 0.0
                cellToNumber (idx, Cell c f) = intToFloat $ figureToNumber f c
    res = execute nn input
  in head res

figureToNumber :: Figure -> Color -> Int
figureToNumber f c = case (f, c) of
                       (Pawn, White) -> 1
                       (Rook, White) -> 2
                       (Knight, White) -> 3
                       (Bishop, White) -> 4
                       (Queen, White) -> 5
                       (King, White) -> 6
                       (Pawn, Black) -> 11
                       (Rook, Black) -> 12
                       (Knight, Black) -> 13
                       (Bishop, Black) -> 14
                       (Queen, Black) -> 15
                       (King, Black) -> 16