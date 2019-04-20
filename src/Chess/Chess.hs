module Chess.Chess where

import Data.Array
import Data.Function
import Control.Monad
import Data.Maybe

data Figure = King | Queen | Bishop | Knight | Rook | Pawn deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)
data Cell = Empty | Cell Color Figure deriving (Show, Eq)
type Board = Array Int Cell
data HorizontalAxis = A | B | C | D | E | F | G | H deriving (Show, Eq, Ord)
data VerticalAxis = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Show, Eq, Ord)
type Position = Int

newBoard :: Board
newBoard = array (1, 64) [(1, Cell White Rook), (2, Cell White Knight), (3, Cell White Bishop), (4, Cell White Queen), (5, Cell White King), (6, Cell White Bishop), (7, Cell White Knight), (8, Cell White Rook),
    (9, Cell White Pawn), (10, Cell White Pawn), (11, Cell White Pawn), (12, Cell White Pawn), (13, Cell White Pawn), (14, Cell White Pawn), (15, Cell White Pawn), (16, Cell White Pawn),
    (17, Empty), (18, Empty), (19, Empty), (20, Empty), (21, Empty), (22, Empty), (23, Empty), (24, Empty),
    (25, Empty), (26, Empty), (27, Empty), (28, Empty), (29, Empty), (30, Empty), (31, Empty), (32, Empty),
    (33, Empty), (34, Empty), (35, Empty), (36, Empty), (37, Empty), (38, Empty), (39, Empty), (40, Empty),
    (41, Empty), (42, Empty), (43, Empty), (44, Empty), (45, Empty), (46, Empty), (47, Empty), (48, Empty),
    (49, Cell Black Pawn), (50, Cell Black Pawn), (51, Cell Black Pawn), (52, Cell Black Pawn), (53, Cell Black Pawn), (54, Cell Black Pawn), (55, Cell Black Pawn), (56, Cell Black Pawn),
    (57, Cell Black Rook), (58, Cell Black Knight), (59, Cell Black Bishop), (60, Cell Black Queen), (61, Cell Black King), (62, Cell Black Bishop), (63, Cell Black Knight), (64, Cell Black Rook)]

emptyBoard :: Board
emptyBoard = array (1, 64) [(1, Empty), (2, Empty), (3, Empty), (4, Empty), (5, Empty), (6, Empty), (7, Empty), (8, Empty),
    (9, Empty), (10, Empty), (11, Empty), (12, Empty), (13, Empty), (14, Empty), (15, Empty), (16, Empty),
    (17, Empty), (18, Empty), (19, Empty), (20, Empty), (21, Empty), (22, Empty), (23, Empty), (24, Empty),
    (25, Empty), (26, Empty), (27, Empty), (28, Empty), (29, Empty), (30, Empty), (31, Empty), (32, Empty),
    (33, Empty), (34, Empty), (35, Empty), (36, Empty), (37, Empty), (38, Empty), (39, Empty), (40, Empty),
    (41, Empty), (42, Empty), (43, Empty), (44, Empty), (45, Empty), (46, Empty), (47, Empty), (48, Empty),
    (49, Empty), (50, Empty), (51, Empty), (52, Empty), (53, Empty), (54, Empty), (55, Empty), (56, Empty),
    (57, Empty), (58, Empty), (59, Empty), (60, Empty), (61, Empty), (62, Empty), (63, Empty), (64, Empty)]

horizontalToNumber A = 1
horizontalToNumber B = 2
horizontalToNumber C = 3
horizontalToNumber D = 4
horizontalToNumber E = 5
horizontalToNumber F = 6
horizontalToNumber G = 7
horizontalToNumber H = 8
verticalToNumber One = 0
verticalToNumber Two = 8
verticalToNumber Three = 16
verticalToNumber Four = 24
verticalToNumber Five = 32
verticalToNumber Six = 40
verticalToNumber Seven = 48
verticalToNumber Eight = 56
numberToHorizontal 1 = Just A
numberToHorizontal 2 = Just B
numberToHorizontal 3 = Just C
numberToHorizontal 4 = Just D
numberToHorizontal 5 = Just E
numberToHorizontal 6 = Just F
numberToHorizontal 7 = Just G
numberToHorizontal 8 = Just H
numberToHorizontal _ = Nothing
numberToVertical 1 = Just One
numberToVertical 2 = Just Two
numberToVertical 3 = Just Three
numberToVertical 4 = Just Four
numberToVertical 5 = Just Five
numberToVertical 6 = Just Six
numberToVertical 7 = Just Seven
numberToVertical 8 = Just Eight
numberToVertical _ = Nothing

positionToCoordinate :: Position -> Maybe (HorizontalAxis, VerticalAxis)
positionToCoordinate position =
  let
    pos = position - 1
    hNum = quot pos 8
    vNum = pos - hNum * 8
  in
    (,) <$> (numberToHorizontal (vNum + 1)) <*> (numberToVertical (hNum + 1))

coordinateToPosition :: HorizontalAxis -> VerticalAxis -> Position
coordinateToPosition h v = (horizontalToNumber h) + (verticalToNumber v)

left :: HorizontalAxis -> Maybe HorizontalAxis
left = numberToHorizontal . (\a -> a - 1) . horizontalToNumber

right :: HorizontalAxis -> Maybe HorizontalAxis
right = numberToHorizontal . (+1) . horizontalToNumber

up :: VerticalAxis -> Maybe VerticalAxis
up = numberToVertical .  (+2) . (flip quot 8) . verticalToNumber

down :: VerticalAxis -> Maybe VerticalAxis
down = numberToVertical .  (flip quot 8) . verticalToNumber

getPosition :: (HorizontalAxis -> Maybe HorizontalAxis) -> (VerticalAxis -> Maybe VerticalAxis) -> Position -> Maybe Position
getPosition hMove vMove p =
  do
    (h, v) <- positionToCoordinate p
    h'     <- hMove h
    v'     <- vMove v
    let res = coordinateToPosition h' v'
    pure res

cellAt :: Board -> Position -> Maybe Cell
cellAt b p
  | p >= 1 && p <= 64 = Just $ b ! p
  | otherwise = Nothing

cellAtCoordinate :: Board -> (HorizontalAxis, VerticalAxis) -> Cell
cellAtCoordinate b (h, v) = b ! coordinateToPosition h v

isEmptyCellAt :: Board -> Position -> Bool
isEmptyCellAt b p = case cellAt b p of
                      Just Empty -> True
                      _ -> False

colorAt :: Board -> Position -> Maybe Color
colorAt b p =
  case (cellAt b p) of
    Just Empty -> Nothing
    Just (Cell White _) -> Just White
    Just (Cell Black _) -> Just Black

figureAt :: Board -> Position -> Maybe Figure
figureAt b p =
  case (cellAt b p) of
    Just Empty -> Nothing
    Just (Cell _ f) -> Just f

inverseColor White = Black
inverseColor Black = White

knightMoves :: Board -> Position -> [Position]
knightMoves b p =
  concat $ maybeToList $ do
    color <- colorAt b p
    let
      allMoves = concat $ (maybeToList . (\f -> f p)) <$> moves where
          moves = [ getPosition (left >=> left) up
                  , getPosition (left >=> left) down
                  , getPosition (right >=> right) up
                  , getPosition (right >=> right) down
                  , getPosition right (up >=> up)
                  , getPosition left (up >=> up)
                  , getPosition right (down >=> down)
                  , getPosition left (down >=> down)
                  ]
      attackMoves = filter (\p -> colorAt b p == Just (inverseColor color)) allMoves
      nonAttackMoves = filter (\p -> colorAt b p == Nothing) allMoves
    pure $ nonAttackMoves ++ attackMoves

kingMoves :: Board -> Position -> [Position]
kingMoves b p =
  concat $ maybeToList $ do
    color <- colorAt b p
    let
      allMoves = concat $ (maybeToList . (\f -> f p)) <$> moves where
                   moves = [ getPosition left pure
                           , getPosition right pure
                           , getPosition pure up
                           , getPosition pure down
                           ]
      attackMoves = filter (\p -> colorAt b p == Just (inverseColor color)) allMoves
      nonAttackMoves = filter (\p -> colorAt b p == Nothing) allMoves
    pure $ nonAttackMoves ++ attackMoves

pawnMoves :: Board -> Position -> [Position]
pawnMoves b p =
  concat $ maybeToList $ do
    color <- colorAt b p
    (h, v) <- positionToCoordinate p
    let extra = filter (\v -> isEmptyCellAt b v) $ case (v, color) of
                  (Two, White) -> if cellAtCoordinate b (h, Three) == Empty then [coordinateToPosition h Four] else []
                  (Seven, Black) -> if cellAtCoordinate b (h, Six) == Empty then [coordinateToPosition h Five] else []
                  _ -> []
        primary = filter (\v -> isEmptyCellAt b v) $ maybeToList $ (case color of
                      White -> getPosition pure up p
                      Black -> getPosition pure down p)
        attack = (case color of
                   White -> rightAttack ++ leftAttack where
                              rightAttack = maybeToList $ mfilter (\v -> (colorAt b v) == (Just Black)) $ getPosition right up p
                              leftAttack = maybeToList $ mfilter (\v -> (colorAt b v) == (Just Black)) $ getPosition left up p
                   Black -> rightAttack ++ leftAttack where
                              rightAttack = maybeToList $ mfilter (\v -> (colorAt b v) == (Just White)) $ getPosition right down p
                              leftAttack = maybeToList $ mfilter (\v -> (colorAt b v) == (Just White)) $ getPosition left down p
                  )
    pure $ primary ++ extra ++ attack

rockMoves :: Board -> Position -> [Position]
rockMoves b p =
  concat $ maybeToList $ do
    color <- colorAt b p
    (h, v) <- positionToCoordinate p
    let moves = (rockMoves' pure up p) ++ (rockMoves' pure down p) ++ (rockMoves' right pure p) ++ (rockMoves' left pure p) where
        rockMoves' :: (HorizontalAxis -> Maybe HorizontalAxis) -> (VerticalAxis -> Maybe VerticalAxis) -> Position -> [Position]
        rockMoves' hM vM p' =
          concat $ maybeToList $ do
            p'' <- getPosition hM vM p'
            let rest = case cellAt b p'' of
                          Just Empty -> p'' : (rockMoves' hM vM p'')
                          Just (Cell color' _) | color' == inverseColor color -> [p'']
                          _ -> []
            pure rest
    pure moves

bishopMoves :: Board -> Position -> [Position]
bishopMoves b p =
  concat $ maybeToList $ do
    color <- colorAt b p
    (h, v) <- positionToCoordinate p
    let moves = (bishopMoves' right up p) ++ (bishopMoves' right down p) ++ (bishopMoves' left up p) ++ (bishopMoves' left down p) where
        bishopMoves' :: (HorizontalAxis -> Maybe HorizontalAxis) -> (VerticalAxis -> Maybe VerticalAxis) -> Position -> [Position]
        bishopMoves' hM vM p' =
          concat $ maybeToList $ do
            p'' <- getPosition hM vM p'
            let rest = case cellAt b p'' of
                          Just Empty -> p'' : (bishopMoves' hM vM p'')
                          Just (Cell color' _) | color' == inverseColor color -> [p'']
                          _ -> []
            pure rest
    pure moves

queenMoves :: Board -> Position -> [Position]
queenMoves b p = rockMoves b p ++ bishopMoves b p

makeMove :: Board -> Position -> Position -> Board
makeMove b from to = b // [(from, Empty), (to, fromCell)] where fromCell = b ! from

setCell :: Board -> Cell -> Position -> Maybe Board
setCell b c pos
  | pos < 1 || pos > 64 = Nothing
  | otherwise = Just $ b // [(pos, c)]

isAttackMove :: Board -> Position -> Position -> Bool
isAttackMove b from to =
  let
    a = (do
          colorFrom <- colorAt b from
          colorTo <- colorAt b to
          pure $ colorFrom /= colorTo)
    res = case a of
            Just True -> True
            _         -> False
  in res

allPossibleMoves :: Color -> Board -> [(Position, Position)]
allPossibleMoves c b = concat $ map allMovesOfFigure (assocs b) where
                          allMovesOfFigure :: (Int, Cell) -> [(Position, Position)]
                          allMovesOfFigure (pos, (Cell col fig)) | col == c =
                              case fig of
                                 Pawn -> posToTuple pos <$> pawnMoves b pos
                                 Rook -> posToTuple pos <$> rockMoves b pos
                                 Bishop -> posToTuple pos <$> bishopMoves b pos
                                 Knight -> posToTuple pos <$> knightMoves b pos
                                 King -> posToTuple pos <$> kingMoves b pos
                                 Queen -> posToTuple pos <$> queenMoves b pos
                          allMovesOfFigure _ = []

                          posToTuple from to = (from, to)

boardPermutations :: Color -> Board -> [(Board, Position, Position)]
boardPermutations c b = (\(from, to) -> (makeMove b from to, from, to)) <$> allPossibleMoves c b

assessPerformance :: Board -> Color -> Float
assessPerformance b c =
  let
    figureWeight Pawn = 2.0
    figureWeight Rook = 12.0
    figureWeight Knight = 8.0
    figureWeight Bishop = 10.0
    figureWeight Queen = 15.0
    figureWeight King = 10000.0

    filterByColor :: Color -> Cell -> Bool
    filterByColor c' (Cell cc ff) | cc == c' = True
    filterByColor _ _ = False

    cellWeight :: Cell -> Float
    cellWeight Empty = 0.0
    cellWeight (Cell _ f) = figureWeight f

    thisColorFigures = filter (filterByColor c) $ elems b
    totalFigureWeight = foldl (+) 0.0 $ map cellWeight thisColorFigures

    attackMoves c' = foldl (+) 0.0 $
                     map attackedFigureWeight $
                     filter isAttack $
                     allPossibleMoves c' b
                     where isAttack (f', t') = isAttackMove b f' t'
                           attackedFigureWeight :: (Position, Position) -> Float
                           attackedFigureWeight (_, to') = (figureWeightAt to') / 2
                           figureWeightAt p' = case cellAt b p' of
                                                 Just (Cell _ f) -> figureWeight f
                                                 _               -> 0.0

    oppositeAttackMoves = attackMoves (inverseColor c)
    ourAttackMoves = attackMoves c
  in totalFigureWeight - oppositeAttackMoves + ourAttackMoves