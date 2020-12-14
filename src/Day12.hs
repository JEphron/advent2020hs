{-# LANGUAGE NamedFieldPuns #-}

module Day12 where

import qualified Data.Char as Char
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read

type Angle = Int

data Ship = Ship
  { lat :: Int,
    lng :: Int,
    dir :: Angle
  }
  deriving (Show)

data Cmd
  = North Int
  | South Int
  | East Int
  | West Int
  | Forward Int
  | RotLeft Int
  | RotRight Int
  deriving (Show)

instance Read Cmd where
  readPrec =
    Text.Read.lift $
      ReadP.choice
        [ parseCmd "N" North,
          parseCmd "S" South,
          parseCmd "E" East,
          parseCmd "W" West,
          parseCmd "F" Forward,
          parseCmd "L" RotLeft,
          parseCmd "R" RotRight
        ]
    where
      parseCmd code cmd =
        cmd <$> (ReadP.string code >> parseNum)
      parseNum =
        read <$> (ReadP.munch1 Char.isNumber)

part1 :: [String] -> String
part1 ss =
  let initial = Ship {lat = 0, lng = 0, dir = 0}
      cmds = map read ss :: [Cmd]
   in unlines $
        map (\(s, c) -> show (s, c, manhattan s)) $
          zip (drop 1 $ scanl updateShip initial cmds) cmds

manhattan :: Ship -> Int
manhattan (Ship {lat, lng}) = abs lat + abs lng

angleClamp n = if n >= 360 then n - 360 else if n < 0 then n + 360 else n

updateShip :: Ship -> Cmd -> Ship
updateShip ship@(Ship {lat, lng, dir}) cmd =
  case cmd of
    North n -> ship {lat = lat + n}
    South n -> ship {lat = lat - n}
    East n -> ship {lng = lng + n}
    West n -> ship {lng = lng - n}
    RotLeft n -> ship {dir = angleClamp (dir + n)}
    RotRight n -> ship {dir = angleClamp (dir - n)}
    Forward n ->
      case dir of
        0 -> ship {lng = lng + n}
        90 -> ship {lat = lat + n}
        180 -> ship {lng = lng - n}
        270 -> ship {lat = lat - n}
        _ -> error (show dir)
