module Day14 where

import Control.Applicative ((<|>))
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read

data MaskBit = Zero | One | DontCare deriving (Eq)

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
  let cmds = map read ss
      state = (Map.empty, [])
      (finalMem, _) = foldl update1 state cmds
   in sum $ Map.elems finalMem

part2 ss =
  let cmds = map read ss
      state = (Map.empty, [])
      (finalMem, _) = foldl update2 state cmds
   in sum $ Map.elems finalMem

update1 :: State -> Cmd -> State
update1 state@(mem, mask) cmd =
  case cmd of
    MaskOp mask' ->
      (mem, mask')
    MemOp addr val ->
      (updateMem state addr val, mask)
  where
    updateMem (mem, mask) addr val =
      let paddedVal = lpad (length mask) False (toBinary val)
          bzoopies = zip mask paddedVal
          bzizzle = map (uncurry applyValMask) bzoopies
       in Map.insert addr (toDecimal bzizzle) mem

applyValMask :: MaskBit -> Bool -> Bool
applyValMask DontCare b = b
applyValMask One _ = True
applyValMask Zero _ = False

update2 :: State -> Cmd -> State
update2 state@(mem, mask) cmd =
  case cmd of
    MaskOp mask' ->
      (mem, mask')
    MemOp addr val ->
      (updateMem state addr val, mask)
  where
    updateMem (mem, mask) addr val =
      let paddedAddr = lpad (length mask) False (toBinary addr)
          addrMask = map (uncurry applyMemMask) $ zip mask paddedAddr
          expandedAddrs = map toDecimal $ expandMask addrMask
       in foldr (flip Map.insert val) mem expandedAddrs

expandMask :: [MaskBit] -> [[Bool]]
expandMask mask =
  let allTheBytes = toBinary <$> [0 .. 2 ^ (count DontCare mask) - 1]

      sqee :: ([Bool], [Bool]) -> MaskBit -> ([Bool], [Bool])
      sqee (perm, res) Zero = (perm, False : res)
      sqee (perm, res) One = (perm, True : res)
      sqee (p : ps, res) DontCare = (ps, p : res)
      sqee ([], res) DontCare = ([], False : res)

      help :: [Bool] -> [[Bool]] -> [[Bool]]
      help perm acc = (reverse $ snd $ foldl sqee (reverse perm, []) mask) : acc
   in foldr help [] allTheBytes

applyMemMask :: MaskBit -> Bool -> MaskBit
applyMemMask Zero False = Zero
applyMemMask Zero True = One
applyMemMask m _ = m

-- utils --

count a = length . filter (== a)

lpad m v xs = replicate (m - length ys) v ++ ys
  where
    ys = take (max m (length xs)) xs

toBinary :: Int -> [Bool]
toBinary 0 = []
toBinary n = toBinary (n `div` 2) ++ [(n `mod` 2 == 1)]

toDecimal :: [Bool] -> Int
toDecimal bits =
  foldl (\acc x -> acc * 2 + fromEnum x) 0 bits
