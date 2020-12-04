module Day4 where

import Data.List.Split
import Data.Map ((!), (!?))
import qualified Data.Map as M
import Data.Maybe
import Lib
import Text.Regex.TDFA

type Passport = M.Map String String

type Field = (String, String -> Bool)

validHgt :: String -> Bool
validHgt str =
  case str =~ "^([0-9]+)(cm|in)$" :: (String, String, String, [String]) of
    (_, _, _, num : "cm" : []) ->
      betwixt 150 193 (read num)
    (_, _, _, num : "in" : []) ->
      betwixt 59 76 (read num)
    _ ->
      False

requiredFields :: [Field]
requiredFields =
  [ ("byr", bounded 1920 2002),
    ("iyr", bounded 2010 2020),
    ("eyr", bounded 2020 2030),
    ("hgt", validHgt),
    ("hcl", (=~ "^#[0-9|a-f]{6}$")),
    ("ecl", (=~ "^amb|blu|brn|gry|grn|hzl|oth$")),
    ("pid", (=~ "^[0-9]{9}$"))
  ]
  where
    bounded lo hi it = betwixt lo hi (read it)

part1 :: [String] -> String
part1 =
  show
    . length
    . filter (validPassport . parsePassport)
    . splitOn "\n\n"
    . unlines

validPassport :: Passport -> Bool
validPassport passport =
  all (validField passport) requiredFields

validField :: Passport -> Field -> Bool
validField passport (fieldName, predicate) =
  fromMaybe False $ predicate <$> passport !? fieldName

parsePassport :: String -> Passport
parsePassport =
  M.fromList
    . mapMaybe makePairs
    . splitOneOf "\n "
  where
    makePairs str =
      case splitOn ":" str of
        a : b : [] -> Just (a, b)
        _ -> Nothing
