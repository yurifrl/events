{-# LANGUAGE OverloadedStrings #-}
module Ingredient ( ingredients ) where

import Data.Text (Text, splitOn, isInfixOf)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (sortBy)

-- |
database :: M.Map Text [Text]
database = M.fromList
  [("Classic", ["strawberry", "banana", "pineapple", "mango", "peach", "honey", "ice", "yogurt"])
  , ( "Forest Berry", ["strawberry", "raspberry", "blueberry", "honey", "ice", "yogurt"])
  , ( "Freezie", ["blackberry", "blueberry", "black currant", "grape juice", "frozen yogurt"])
  , ( "Greenie", ["green apple", "kiwi", "lime", "avocado", "spinach", "ice", "apple juice"])
  , ( "Vegan Delite", ["strawberry", "passion fruit", "pineapple", "mango", "peach", "ice", "soy milk"])
  , ( "Just Desserts", ["banana", "ice cream", "chocolate", "peanut", "cherry"])]

-- | given list returns True if the list contains an item
containsItem :: [Text] -> Text -> Bool
containsItem = flip elem

-- | split text on separator
splitOnSeparator :: Text -> [Text]
splitOnSeparator = splitOn ","

-- | get key from the list
getKey :: Text -> Text
getKey = head . splitOnSeparator

-- | get key from the list
getValues :: Text -> [Text]
getValues = tail . splitOnSeparator

-- | get item from list that matches prefix and returns whitout prefix
extractItems :: Char -> Text -> [Text] ->[Text]
extractItems i x xs | (isInfixOf (T.singleton i) x) = (T.dropWhile (==i) x) : xs
                    | otherwise = xs
-- |
getIncludedItems :: [Text] -> [Text]
getIncludedItems = foldr (extractItems '+') []

-- | get excluded items from list
getExcludeItems :: [Text] -> [Text]
getExcludeItems = foldr (extractItems '-') []

-- | checks if item contains in list
isExcludedItem :: [Text] -> Text -> Bool
isExcludedItem t = not . containsItem (getExcludeItems t)

-- | searchDatabase returns database item
searchDatabase :: Ord k => M.Map k [Text] -> k -> [Text]
searchDatabase db item = M.findWithDefault [] item db

-- let
--   x = asdf
-- in
--   ...

-- | ingredients return the ingredients for a given recipe
ingredients :: Text -> [Text]
ingredients t = sortBy compare . (++ includedItems) $ filter f db
  where
    f = isExcludedItem (getValues t)
    db = searchDatabase database (getKey t)
    includedItems = getIncludedItems $ getValues t
