module Game where

data Cell = Wall
          | Empty
          | Pill
          | PowerPill
          | Fruit
          | LambdaMan
          | Ghost
          deriving Enum

instance Show Cell where
        show Wall = "#"
        show Empty = " "
        show Pill = "."
        show PowerPill = "o"
        show Fruit = "%"
        show LambdaMan = "\\"
        show Ghost = "="

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
stage = _

run :: Int -> Game -> [GameState]
run timeout game = _


level1 :: Map
level1 =
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


{-display :: Game -> IO ()-}
{-display -}
