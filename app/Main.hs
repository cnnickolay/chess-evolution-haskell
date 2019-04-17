module Main where

import Chess.NeuralNetwork
import Control.Monad
import Control.Monad.Trans.State
import Chess.Chess
import Chess.ChessMaster
import Data.List

main :: IO ()
main = do
          putStrLn "started..."
          let
            wNNOrig = network [64, 64, 64, 64, 64, 1]
            bNNOrig = network [64, 64, 64, 64, 64, 1]

          (wNNOnSteroids, bNNOnSteroids) <- foldM (\b i -> (trainCycle b) <* (putStrLn $ "Cycle " ++ show i ++ " done")) (wNNOrig, bNNOrig) $ [1..1000]
--          (wNNOnSteroids, bNNOnSteroids) <- trainCycle (wNNOrig, bNNOrig)

          let Game _ moves _ = execState (play wNNOnSteroids bNNOnSteroids 100) newGame
          putStrLn $ show wNNOnSteroids
          putStrLn $ show bNNOnSteroids
          putStrLn $ convertToPGN moves
          pure ()

trainCycle :: (NN, NN) -> IO (NN, NN)
trainCycle (wNNOrig, bNNOrig) =
  do
    wNNGen <- breedNN 0.01 100 wNNOrig
    bNNGen <- breedNN 0.01 100 bNNOrig
    let
        maxMoves = 50
        whiteGames = sortByPerformance $
                      map (\wNN -> (wNN, assessPerformance' $ execState (play wNN bNNOrig maxMoves) newGame)) wNNGen where
                      assessPerformance' :: Game -> (Float, Board, [Move])
                      assessPerformance' (Game b m (Just White)) = (9999999.0, b, m)
                      assessPerformance' (Game b m (Just Black)) = (-9999999.0, b, m)
                      assessPerformance' (Game b m _) = (assessPerformance b White, b, m)
        (top5WhitePlayers, _) = unzip $ take 5 whiteGames
        whiteMutant = head top5WhitePlayers -- squashNNs top5WhitePlayers
        blackGames = sortByPerformance $
                      (\bNN -> (bNN, assessPerformance' $ execState (play whiteMutant bNN maxMoves) newGame)) <$> bNNGen where
                      assessPerformance' :: Game -> (Float, Board, [Move])
                      assessPerformance' (Game b m (Just White)) = (-9999999.0, b, m)
                      assessPerformance' (Game b m (Just Black)) = (9999999.0, b, m)
                      assessPerformance' (Game b m _) = (assessPerformance b Black, b, m)
        (top5BlackPlayers, _) = unzip $ take 5 blackGames
        blackMutant = head top5BlackPlayers -- squashNNs top5BlackPlayers
    pure (whiteMutant, blackMutant)

sortByPerformance :: [(NN, (Float, Board, [Move]))] -> [(NN, (Float, Board, [Move]))]
sortByPerformance l = sortBy comparePerformance l where
                             comparePerformance (_, (a, _, _)) (_, (b, _, _)) | a == b = EQ
                                                                              | a > b = LT
                                                                              | a < b = GT

convertToPGN :: [Move] -> String
convertToPGN ms = foldl (++) "" $ toText <$>
                    zip [1..] ms where
                                   toText (idx, move) = show idx ++ ". " ++ (moveToText $ from move) ++ " " ++ (moveToText $ to move) ++ " "
                                   moveToText (h, v) = horizontalToChar h ++ verticalToChar v
                                   horizontalToChar A = "a"
                                   horizontalToChar B = "b"
                                   horizontalToChar C = "c"
                                   horizontalToChar D = "d"
                                   horizontalToChar E = "e"
                                   horizontalToChar F = "f"
                                   horizontalToChar G = "g"
                                   horizontalToChar H = "h"
                                   verticalToChar One = "1"
                                   verticalToChar Two = "2"
                                   verticalToChar Three = "3"
                                   verticalToChar Four = "4"
                                   verticalToChar Five = "5"
                                   verticalToChar Six = "6"
                                   verticalToChar Seven = "7"
                                   verticalToChar Eight = "8"
