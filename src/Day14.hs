module Day14 where

import Control.Applicative ((<|>))
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read

data MaskBit = Zero | One | DontCare

instance Show MaskBit where
  show Zero = "0"
  show One = "1"
  show DontCare = "X"

data Cmd = MaskOp [MaskBit] | MemOp Int Int

instance Show Cmd where
  show (MaskOp maskBits) =
    "mask = " ++ concatMap show maskBits
  show (MemOp addr value) =
    "mem[" ++ (show addr) ++ "] = " ++ (show value)

instance Read Cmd where
  readPrec =
    Text.Read.lift $
      (parseMaskCmd <|> parseMemCmd)
    where
      parseMaskCmd =
        MaskOp <$> (ReadP.string "mask = " >> ReadP.many parseMaskBit)
      parseMaskBit =
        (ReadP.char 'X' >> return DontCare)
          <|> (ReadP.char '0' >> return Zero)
          <|> (ReadP.char '1' >> return One)
      parseMemCmd =
        MemOp
          <$> (ReadP.string "mem[" >> parseNum)
          <*> (ReadP.string "] = " >> parseNum)
      parseNum =
        read <$> (ReadP.munch1 Char.isNumber)

type State =
  (Map Int Int, [MaskBit])

part1 ss =
  let cmds = map read ss :: [Cmd]
      state = (Map.empty, []) :: State
      (finalMem, _) = foldl update state cmds
   in sum $ Map.elems finalMem

update :: State -> Cmd -> State
update state@(mem, mask) cmd =
  case cmd of
    MaskOp mask' ->
      (mem, mask')
    MemOp addr val ->
      (updateMem state addr val, mask)

updateMem :: State -> Int -> Int -> Map Int Int
updateMem (mem, mask) addr val =
  let bin = lpad (length mask) False (toBinary val)
      bzoopies = zip mask bin
      bzizzle = map (uncurry applyMask) (bzoopies)
   in Map.insert addr (toDecimal bzizzle) mem

applyMask :: MaskBit -> Bool -> Bool
applyMask DontCare b = b
applyMask One _ = True
applyMask Zero _ = False

lpad m v xs = replicate (m - length ys) v ++ ys
  where
    ys = take m xs

toBinary :: Int -> [Bool]
toBinary 0 = [False]
toBinary n = toBinary (n `div` 2) ++ [(n `mod` 2 == 1)]

toDecimal :: [Bool] -> Int
toDecimal bits =
  foldl (\acc x -> acc * 2 + fromEnum x) 0 bits
