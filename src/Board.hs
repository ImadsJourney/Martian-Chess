module Board where -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars

import Control.Monad.ST (fixST)
import Data.Char ()
import Text.Read ()

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Given data types may NOT be changed            ################
-- #############################################################################

data Player = Top | Bottom deriving (Show, Read)

data Cell = Empty | Queen | Drone | Pawn deriving (Show)

data Pos = Pos {col :: Char, row :: Int} deriving (Show)

type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Top Top = True
  (==) Bottom Bottom = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) Pawn Pawn = True
  (==) Drone Drone = True
  (==) Queen Queen = True
  (==) _ _ = False

startingFEN :: String
startingFEN = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"

startingBoard :: [[Cell]]
startingBoard =
  [ [Queen, Queen, Drone, Empty],
    [Queen, Drone, Pawn, Empty],
    [Drone, Pawn, Pawn, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Pawn, Pawn, Drone],
    [Empty, Pawn, Drone, Queen],
    [Empty, Drone, Queen, Queen]
  ]

buildPos :: String -> Pos
buildPos (c : rStr) = Pos c (read rStr)
buildPos _ = Pos 'a' 0

-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

validateFEN :: String -> Bool
validateFEN s = let rows = splitFEN s in length rows == 8 && all validRow rows

splitFEN :: String -> [String]
splitFEN "" = [""]
splitFEN s = case break (== '/') s of
  (beg, "") -> [beg]
  (beg, _ : xs) -> beg : splitFEN xs

countFields :: String -> Int
countFields "" = 0
countFields (x : xs)
  | x `elem` "pdq" = 1 + countFields xs
  | x `elem` "123" = read [x] + countFields xs
  | otherwise = -1

validRow :: String -> Bool
validRow "" = True
validRow row =
  all (`elem` allowed) row && any (`elem` "pdq") row && countFields row == 4
  where
    allowed = "pdq123"

-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildBoard :: String -> Board
buildBoard fen = map rowToCells (splitFEN fen)
  where
    rowToCells "" = replicate 4 Empty
    rowToCells r = buildRow r

buildRow :: String -> [Cell]
buildRow [] = []
buildRow (x : xs)
  | x == 'p' = Pawn : buildRow xs
  | x == 'd' = Drone : buildRow xs
  | x == 'q' = Queen : buildRow xs
  | x `elem` "123" = replicate (read [x]) Empty ++ buildRow xs
  | otherwise = error $ "Invalid character in FEN: " ++ [x]

-- ##############################################################################
-- ################## IMPLEMENT buildFEN :: Board -> String   ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildFEN :: Board -> String
buildFEN board = concatWithSlash (map rowToFEN board)

rowToFEN :: [Cell] -> String
rowToFEN row
  | all (== Empty) row = ""
  | otherwise = go row 0
  where
    go :: [Cell] -> Int -> String
    go [] emptyCount = flushEmpties emptyCount
    go (c : cs) emptyCount =
      case c of
        Empty -> go cs (emptyCount + 1)
        _ -> flushEmpties emptyCount ++ cellChar c : go cs 0

    flushEmpties :: Int -> String
    flushEmpties 0 = ""
    flushEmpties n = show n

cellChar :: Cell -> Char
cellChar Pawn = 'p'
cellChar Drone = 'd'
cellChar Queen = 'q'
cellChar Empty = error "Empty should be handled by empty counter"

concatWithSlash :: [String] -> String
concatWithSlash = foldr1 (\r acc -> r ++ "/" ++ acc)
