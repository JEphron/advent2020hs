{-# LANGUAGE NamedFieldPuns #-}

module Day8 where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read

data OpCode = NoOp | Jmp Int | Acc Int deriving (Show)

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
        [ (const NoOp) <$> parseOpCode "nop",
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
part1 s = show $ acc $ State.execState (runCode $ map read s) initProgramState

runCode :: [OpCode] -> State ProgramState ()
runCode program = do
  ProgramState {acc, instruction, visited} <- State.get
  when (Set.notMember instruction visited) $ do
    let visited' = Set.insert instruction visited
    State.put $
      case program !! instruction of
        NoOp ->
          ProgramState acc (instruction + 1) visited'
        Acc n ->
          ProgramState (acc + n) (instruction + 1) visited'
        Jmp n ->
          ProgramState acc (instruction + n) visited'
    runCode program
