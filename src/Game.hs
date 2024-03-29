module Game where

import Control.Monad
import UI.NCurses
import qualified Data.Vector as V

data Cell = Wall
          | Empty
          | Pill
          | PowerPill
          | Fruit
          | LambdaMan
          | Ghost
          deriving (Enum, Eq)

instance Show Cell where
    show c = [cellToChar c]

charToCell '#' = Wall
charToCell ' ' = Empty
charToCell '.' = Pill
charToCell 'o' = PowerPill
charToCell '%' = Fruit
charToCell '\\' = LambdaMan
charToCell '=' = Ghost

cellToChar Wall = '#'
cellToChar Empty = ' '
cellToChar Pill = '.'
cellToChar PowerPill = 'o'
cellToChar Fruit = '%'
cellToChar LambdaMan = '\\'
cellToChar Ghost = '='


data GameState = GameState Tick Map Points

type Map = [[Cell]]

type Points = Int

type Tick = Int

type LambdaManAI = ()
type GhostAI = ()

newtype Game = Game { unGame :: GameState -> (GameState, Maybe Game) }

noMoves :: GameState -> Game
noMoves m = Game (\_ -> (m, Just (noMoves m)))

stage :: LambdaManAI -> [GhostAI] -> GameState -> Game
stage = error "wef"

run :: Int -> Game -> [GameState]
run timeout game = error "wef"


level1 :: Map
level1 = map (map charToCell) $
    "#######################" :
    "#..........#..........#" :
    "#.###.####.#.####.###.#" :
    "#o###.####.#.####.###o#" :
    "#.....................#" :
    "#.###.#.#######.#.###.#" :
    "#.....#....#....#.....#" :
    "#####.#### # ####.#####" :
    "#   #.#    =    #.#   #" :
    "#####.# ### ### #.#####" :
    "#    .  # === #  .    #" :
    "#####.# ####### #.#####" :
    "#   #.#    %    #.#   #" :
    "#####.# ####### #.#####" :
    "#..........#..........#" :
    "#.###.####.#.####.###.#" :
    "#o..#......\\......#..o#" :
    "###.#.#.#######.#.#.###" :
    "#.....#....#....#.....#" :
    "#.########.#.########.#" :
    "#.....................#" :
    "#######################" :
    []

level2 :: Map
level2 = map (map charToCell) $
    "#######################" :
    "#..........#..........#" :
    "#.###.####.#.####.###.#" :
    "#o###.####.#.####.###o#" :
    "#.....................#" :
    "#.###.#.#######.#.###.#" :
    "#.....#....#....#.....#" :
    "#####.#### # ####.#####" :
    "#   #.#    =    #.#   #" :
    "#####.# ### ### #.#####" :
    "#    .  # === #  .    #" :
    "#####.# ####### #.#####" :
    "#   #.#      %  #.#   #" :
    "#####.# ####### #.#####" :
    "#..........#..........#" :
    "#.###.####.#.####.###.#" :
    "#o..#.....\\.......#..o#" :
    "###.#.#.#######.#.#.###" :
    "#.....#....#....#.....#" :
    "#.########.#.########.#" :
    "#.....................#" :
    "#######################" :
    []


{-display :: Game -> IO ()-}
{-display -}



type Level = Int

score :: Int -> [Cell] -> Map -> Int -> Int -> Int
score previousScore eatenThings gameMap ghostsEatenNow remainingLives = newScore * sweepBonus
  where
    newScore = (10 * length (filter (== Pill) eatenThings))
             + (50 * length (filter (== PowerPill) eatenThings))
             + (fruitValue * length (filter (== Fruit) eatenThings))
             + (frightModeBonus * length (filter (== Ghost) eatenThings))

    fruitValue = case mapLevel gameMap of
        1  ->  100
        2  ->  300
        3  ->  500
        4  ->  500
        5  ->  700
        6  ->  700
        7  -> 1000
        8  -> 1000
        9  -> 2000
        10 -> 2000
        11 -> 3000
        12 -> 3000
        _  -> 5000

    frightModeBonus = 200 * 2 ^ (min 3 ghostsEatenNow)

    sweepBonus = if null . filter (`elem` [Pill]) . join $ gameMap
                   then remainingLives + 1
                   else 1
                 -- FIXME: Pills, or also PowerPills?

mapLevel :: Map -> Level
mapLevel m = head [ level | level <- [1..]
                          , 100 * (level - 1) < mapWidth * mapHeight && mapWidth * mapHeight <= 100 * level ]
  where
    mapWidth = length (head m)
    mapHeight = length m


video :: V.Vector Map -> IO ()
video states = runCurses $ do
    w <- defaultWindow

    let show_ = unlines . map concat . map (map show)

        loop i
          | i < 0                 = loop 0
          | i >= V.length states  = loop (V.length states - 1)
          | True = do
            updateWindow w $ do
                moveCursor 0 0
                drawString . show_ $ states V.! i
            render
            (Just ev) <- getEvent w Nothing
            case ev of
                EventCharacter 'q' -> return ()
                EventCharacter 'Q' -> return ()
                EventSpecialKey KeyLeftArrow -> loop (i-1)
                EventSpecialKey KeyRightArrow -> loop (i+1)
                _ -> loop i

    loop 0


{-
main :: IO ()
main = video $ V.fromList [level1, level2]
-}
