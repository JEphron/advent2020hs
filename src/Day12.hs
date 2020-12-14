{-# LANGUAGE NamedFieldPuns #-}

module Day12 where

import qualified Data.Char as Char
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read

type Angle = Int

data Ship = Ship
  { lat :: Int,
    lng :: Int,
    wlat :: Int,
    wlng :: Int,
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
  let initial = Ship {lat = 0, lng = 0, wlat = 1, wlng = 10, dir = 0}
      cmds = map read ss :: [Cmd]
   in unlines $
        map (\(s, c) -> show (s, c, manhattan s)) $
          zip (drop 1 $ scanl updateShip initial cmds) cmds

manhattan :: Ship -> Int
manhattan (Ship {lat, lng}) =
  abs lat + abs lng

angleClamp n =
  if n >= 360 then n - 360 else if n < 0 then n + 360 else n

updateShip :: Ship -> Cmd -> Ship
updateShip ship@(Ship {lat, lng, wlat, wlng, dir}) cmd =
  case cmd of
    North n -> ship {wlat = wlat + n}
    South n -> ship {wlat = wlat - n}
    East n -> ship {wlng = wlng + n}
    West n -> ship {wlng = wlng - n}
    RotLeft n -> rotate ship rot90 n
    RotRight n -> rotate ship rot90cc n
    Forward n -> ship {lat = lat + wlat * n, lng = lng + wlng * n}

rotate ship@(Ship {wlat, wlng}) fn n =
  let (wlat', wlng') = iterate fn (wlat, wlng) !! (n `div` 90)
   in ship {wlng = wlng', wlat = wlat'}

rot90 (x, y) = (y, - x)

rot90cc (x, y) = (- y, x)
