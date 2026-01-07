module Logic where -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char

import Board
import Data.Char (chr, ord)
import Data.Maybe ()

data Move = Move {start :: Pos, target :: Pos}

instance Show Move where
  show (Move (Pos startC startR) (Pos targetC targetR)) = [startC] ++ show startR ++ "-" ++ [targetC] ++ show targetR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1)) (Move (Pos sc2 sr2) (Pos tc2 tr2)) =
    sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2

buildMove :: String -> Maybe Move
buildMove "" = Nothing
buildMove s = case break (== '-') s of
  (a, '-' : b) -> Just (Move (buildPos a) (buildPos b))
  _ -> Nothing

-- ########################################################################################################
-- ################## pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]        ##################
-- ################## - 5 Functional Points                                              ##################
-- ########################################################################################################

pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
pawnMoves b pl from lastMv =
  case getCell b from of
    Empty -> []
    piece ->
      if zoneOf from /= pl
        then []
        else
          let noQueens = countInZone b pl Queen == 0
              noDrones = countInZone b pl Drone == 0
              ms =
                case piece of
                  Pawn -> pawnFrom b pl noQueens noDrones from
                  Drone -> droneFrom b pl noQueens from
                  Queen -> queenFrom b pl from
                  _ -> []
           in filter (not . isRollback lastMv) ms

-- helper methods

getCell :: Board -> Pos -> Cell
getCell board (Pos c r) =
  let rowIx = 7 - r
      colIx = ord c - ord 'a'
   in (board !! rowIx) !! colIx

setCell :: Board -> Pos -> Cell -> Board
setCell board (Pos c r) newCell =
  let rowIx = 7 - r
      colIx = ord c - ord 'a'

      oldRow = board !! rowIx
      newRow = take colIx oldRow ++ [newCell] ++ drop (colIx + 1) oldRow
   in take rowIx board ++ [newRow] ++ drop (rowIx + 1) board

inBounds :: Pos -> Bool
inBounds (Pos c r) = c >= 'a' && c <= 'd' && r >= 0 && r <= 7

stepPos :: Pos -> (Int, Int) -> Int -> Pos
stepPos (Pos c r) (dx, dy) n =
  Pos (chr (ord c + dx * n)) (r + dy * n)

countPiecesInRay :: Board -> Pos -> (Int, Int) -> Int
countPiecesInRay b from dir = go 1 0
  where
    go n acc =
      let p = stepPos from dir n
       in if not (inBounds p)
            then acc
            else
              let cell = getCell b p
                  acc' = if cell == Empty then acc else acc + 1
               in go (n + 1) acc'

zoneOf :: Pos -> Player
zoneOf (Pos _ r) = if r >= 4 then Top else Bottom

isRollback :: Maybe Move -> Move -> Bool
isRollback Nothing _ = False
isRollback (Just (Move s0 t0)) (Move s1 t1) =
  crossedCanal s0 t0 && s1 == t0 && t1 == s0

crossedCanal :: Pos -> Pos -> Bool
crossedCanal a b = zoneOf a /= zoneOf b

pawnFrom :: Board -> Player -> Bool -> Bool -> Pos -> [Move]
pawnFrom b pl noQueens noDrones from =
  concatMap step [(-1, -1), (-1, 1), (1, -1), (1, 1)]
  where
    step dir =
      let to = stepPos from dir 1
       in if not (inBounds to)
            then []
            else
              let dest = getCell b to
                  sameZone = zoneOf to == pl
               in if sameZone
                    then case dest of
                      Empty -> [Move from to]
                      Pawn -> if noDrones then [Move from to] else []
                      Drone -> if noQueens then [Move from to] else []
                      Queen -> []
                    else case dest of
                      Empty -> [Move from to]
                      _ -> [Move from to]

droneFrom :: Board -> Player -> Bool -> Pos -> [Move]
droneFrom b pl noQueens from =
  concatMap dirMoves [(-1, 0), (1, 0), (0, -1), (0, 1)]
  where
    dirMoves dir =
      let lim = max 1 (countPiecesInRay b from dir)
       in scan dir 1 lim

    scan _ n lim | n > lim = []
    scan dir n lim =
      let to = stepPos from dir n
       in if not (inBounds to)
            then []
            else
              let dest = getCell b to
                  sameZone = zoneOf to == pl
               in case dest of
                    Empty -> Move from to : scan dir (n + 1) lim
                    _ ->
                      if sameZone
                        then if dest == Pawn && noQueens then [Move from to] else []
                        else [Move from to]

queenFrom :: Board -> Player -> Pos -> [Move]
queenFrom b pl from =
  concatMap scanDir dirs
  where
    dirs = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]
    scanDir dir = go 1
      where
        go n =
          let to = stepPos from dir n
           in if not (inBounds to)
                then []
                else
                  let dest = getCell b to
                      sameZone = zoneOf to == pl
                   in case dest of
                        Empty -> Move from to : go (n + 1)
                        _ -> if sameZone then [] else [Move from to]

countInZone :: Board -> Player -> Cell -> Int
countInZone board pl wanted =
  length
    [ ()
      | r <- [0 .. 7],
        c <- ['a' .. 'd'],
        let p = Pos c r,
        zoneOf p == pl,
        getCell board p == wanted
    ]

-- #######################################################################################################
-- ################## droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 5 Functional Points                                             ##################
-- #######################################################################################################

droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
droneMoves b pl from lastMv =
  case getCell b from of
    Drone ->
      if zoneOf from /= pl
        then []
        else
          let noQueens = countInZone b pl Queen == 0
              ms = droneFrom b pl noQueens from
           in filter (not . isRollback lastMv) ms
    _ -> []

-- #######################################################################################################
-- ################## queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
queenMoves b pl from lastMv =
  case getCell b from of
    Queen ->
      if zoneOf from /= pl
        then []
        else
          let ms = queenFrom b pl from
           in filter (not . isRollback lastMv) ms
    _ -> []

-- #######################################################################################################
-- ################## makeMove :: Board -> Move -> (Board -> Int)                       ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

makeMove :: Board -> Move -> (Board, Int)
makeMove board (Move s t) =
  let moving = getCell board s
      dest = getCell board t

      board1 = setCell board s Empty

      cross = zoneOf s /= zoneOf t
      points = if cross then pieceValue dest else 0

      finalPiece =
        if cross
          then moving
          else case (moving, dest) of
            (Pawn, Pawn) -> Drone
            (Pawn, Drone) -> Queen
            (Drone, Pawn) -> Queen
            _ -> moving -- fallback: treat as normal move
   in (setCell board1 t finalPiece, points)

pieceValue :: Cell -> Int
pieceValue Pawn = 1
pieceValue Drone = 2
pieceValue Queen = 3
pieceValue Empty = 0

-- #######################################################################################################
-- ################## playerWon :: Board -> Player -> Int -> Int -> Maybe Player        ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

playerWon :: Board -> Player -> Int -> Int -> Maybe Player
playerWon board lastPlayer topScore bottomScore
  | not (zoneEmpty board Top || zoneEmpty board Bottom) = Nothing
  | topScore > bottomScore = Just Top
  | bottomScore > topScore = Just Bottom
  | otherwise = Just lastPlayer

zoneEmpty :: Board -> Player -> Bool
zoneEmpty board pl =
  null
    [ ()
      | r <- [0 .. 7],
        c <- ['a' .. 'd'],
        let p = Pos c r,
        zoneOf p == pl,
        getCell board p /= Empty
    ]
