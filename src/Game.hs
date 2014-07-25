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
        show LambdaMan = "\"
        show Ghost = "="

type Map = [[Cell]]
type Tick = Int

type Move = Map -> Map
