{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

type Thing = (Int, Text)

type Bags = Map Text [Thing]

part1 =
  show
    . length
    . Set.fromList
    . countCanContain "shiny gold"
    . invertBagMap
    . foldl mappify Map.empty
    . map parseBagRule
    . map T.pack

part2 =
  show
    . bagContents "shiny gold"
    . foldl mappify Map.empty
    . map parseBagRule
    . map T.pack

invertBagMap :: Bags -> Bags
invertBagMap bags =
  Map.fromListWith (<>) $ Map.assocs bags >>= (\(key, values) -> screw key <$> values)
  where
    screw key (n, t) = (t, [(n, key)])

countCanContain :: Text -> Bags -> [Text]
countCanContain name bags =
  case bags !? name of
    Just subs ->
      map snd subs
        <> ((\(n, subName) -> countCanContain subName bags) =<< subs)
    Nothing -> []

bagContents :: Text -> Bags -> Int
bagContents name bags =
  subContents name bags - 1
  where
    subContents name bags =
      1 + (sum $ map (\(count, name') -> count * subContents name' bags) (bags ! name))

mappify :: Bags -> (Text, [Thing]) -> Bags
mappify bags (bagName, subBags) = Map.insert bagName subBags bags

parseBagRule :: Text -> (Text, [Thing])
parseBagRule s =
  let name : contents =
        replaceAll [(" bags", ""), (" bag", ""), (".", "")]
          <$> ((T.splitOn " contain " s) >>= (T.splitOn ", "))
   in ( name,
        if T.isInfixOf "no other" (head contents)
          then []
          else map extractNumber contents
      )
  where
    extractNumber txt =
      let (x : xs) = T.words txt
       in ( read (T.unpack x),
            T.unwords xs
          )

replaceAll :: [(Text, Text)] -> Text -> Text
replaceAll ((from, to) : xs) txt = replaceAll xs $ T.replace from to txt
replaceAll [] txt = txt
