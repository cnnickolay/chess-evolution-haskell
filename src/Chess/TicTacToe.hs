module Chess.TicTacToe where

import Control.Monad
import Control.Monad.Trans.State
import Data.Array
import Data.List.Split
import Data.List

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