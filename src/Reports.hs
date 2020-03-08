module Reports (Event(..), Aggregate(..), aggregate) where

import Data.List.Split (splitOn)
import Data.List (intercalate)

type PaymentMethod = String

-- date amount method merchantID
data Event = Event String Int PaymentMethod String
  deriving (Eq, Ord, Read)

-- datapoint events
data Aggregate = Agg String Int
  deriving (Eq, Show)

-- normalize hour string
hour :: Event -> String
hour (Event date _ _ _) = let (day, 'T':time) = break (== 'T') date
                           in day ++ ":" ++ take 2 time
-- normalize day string
day :: Event -> String
day (Event date _ _ _) = takeWhile (/= 'T') $ date

-- separate ammount into brackets
amountBracket :: Event -> String
amountBracket (Event _ amount _ _) | amount < 1000 = "<10"
                                   | amount < 5000 = "10-50"
                                   | amount < 10000 = "50-100"
                                   | amount < 50000 = "100-500"
                                   | otherwise = ">500"

-- check if datapoint is equal aggregate datapoint
matchDatapoint :: String -> Aggregate -> Bool
matchDatapoint datapoint (Agg dp _) = dp == datapoint

-- bump the aggregate event if the datapoint is the same
bumpAggregate :: String -> Aggregate -> Aggregate
bumpAggregate datapoint (Agg dp events) | dp == datapoint = Agg dp (events + 1)
bumpAggregate datapoint (Agg dp events) | dp /= datapoint = Agg dp events

-- loop through aggregates and bump the ones that match datapoint
buildAggregates :: String -> [Aggregate] -> [Aggregate]
buildAggregates datapoint aggrs = map build range
  where build = bumpAggregate datapoint . (aggrs !!)
        range = [0..length aggrs - 1]

-- apply new aggreate into the aggregates list
addAggregate :: String -> [Aggregate] -> [Aggregate]
addAggregate datapoint aggrs | any (matchDatapoint datapoint) aggrs = buildAggregates datapoint aggrs
addAggregate datapoint aggrs | otherwise = aggrs ++ [Agg datapoint 1]

-- breaks event into it's aggreate
eventFold :: Event -> [Aggregate] -> [Aggregate]
eventFold event@(Event _ _ paymentMethod merchantID) acc =
  addAggregate (intercalate "|" [hour event, amountBracket event]) $
  addAggregate (intercalate "|" [hour event, amountBracket event, paymentMethod]) $
  addAggregate (intercalate "|" [amountBracket event, paymentMethod]) $
  addAggregate (intercalate "|" [day event, merchantID]) $
  addAggregate (intercalate "|" [merchantID, paymentMethod]) $
  acc

-- given events matches by it's format and returns the aggregate list
aggregate :: [Event] -> [Aggregate]
aggregate events = foldr eventFold [] events
