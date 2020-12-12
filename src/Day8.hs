{-# LANGUAGE NamedFieldPuns #-}

module Day8 where

import Control.Applicative
import Control.Monad (void, when)
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read

data OpCode = NoOp Int | Jmp Int | Acc Int deriving (Show, Eq)

data ProgramState = ProgramState
  { acc :: Int,
    instruction :: Int,
    visited :: Set Int
  }
  deriving (Show)

instance Read OpCode where
  readPrec =
    Text.Read.lift $
      ReadP.choice
        [ NoOp <$> parseOpCode "nop",
          Jmp <$> parseOpCode "jmp",
          Acc <$> parseOpCode "acc"
        ]
    where
      parseOpCode codeName =
        ReadP.string codeName
          >> ReadP.skipSpaces
          >> parseNum

      parseNum = do
        plusOrMinus <- ReadP.string "-" <|> (const "" <$> ReadP.string "+")
        num <- ReadP.munch1 Char.isNumber
        return $ read (plusOrMinus <> num)

initProgramState :: ProgramState
initProgramState =
  ProgramState
    { acc = 0,
      instruction = 0,
      visited = Set.empty
    }

part1 :: [String] -> String
part1 s = show $ acc $ snd $ runIt (map read s)

runIt :: [OpCode] -> (Bool, ProgramState)
runIt it = State.runState (runProgram it) initProgramState

part2 :: [String] -> String
part2 s =
  let showThing (completed, ProgramState {acc, instruction}) =
        show completed ++ " " ++ show acc ++ " " ++ show instruction
   in unlines $ map showThing $ map runIt $ generateProgramVariants $ take 1000 $ map read s

generateProgramVariants :: [OpCode] -> [[OpCode]]
generateProgramVariants initialProgram =
  let swapOp op =
        case op of
          NoOp n -> Jmp n
          Jmp n -> NoOp n
          x -> x

      makeVariant :: [OpCode] -> Int -> Maybe [OpCode]
      makeVariant program index =
        let variant = applyAt index swapOp program
         in if variant == program then Nothing else Just variant

      pairs :: [([OpCode], Int)]
      pairs = zip (repeat $ initialProgram) [0 .. length initialProgram]
   in Maybe.mapMaybe (uncurry makeVariant) pairs

runProgram :: [OpCode] -> State ProgramState Bool
runProgram program = do
  ProgramState {acc, instruction, visited} <- State.get

  let programSuccess = instruction > length program
  let endOfProgram = Set.member instruction visited || programSuccess

  when (not endOfProgram) $ do
    let visited' = Set.insert instruction visited
    case runOp program instruction acc visited' of
      -- screw it
      Nothing -> error (show (traceShowId acc))
      Just newState -> State.put newState
    void $ runProgram program

  return programSuccess

listSetAt :: Int -> a -> [a] -> [a]
listSetAt i value list =
  applyAt i (const value) list

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt i fn list =
  if i > length list || i < 0
    then list
    else case List.splitAt i list of
      (a, b : bs) ->
        a ++ [fn b] ++ bs
      _ ->
        list

runOp program instruction acc visited' =
  if instruction >= length program
    then Nothing
    else Just $
      case program !! instruction of
        NoOp _ ->
          ProgramState acc (instruction + 1) visited'
        Acc n ->
          ProgramState (acc + n) (instruction + 1) visited'
        Jmp n ->
          ProgramState acc (instruction + n) visited'
