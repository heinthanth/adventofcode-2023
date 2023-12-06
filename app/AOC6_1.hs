module AOC6_1 where

import Data.Text (pack, splitOn, unpack)
import Flow ((|>))
import Debug.Trace (traceShow)

mapTimeAndDistance :: String -> [(Int, Int)]
mapTimeAndDistance gamesData =
  zip timeData distanceData
  where
    rawData =
      gamesData
        |> pack
        |> splitOn (pack "\n")
    timeData =
      head rawData
        |> splitOn (pack " ")
        |> filter (/= pack "")
        |> drop 1
        |> map (\x -> read (unpack x) :: Int)
    distanceData =
      last rawData
        |> splitOn (pack " ")
        |> filter (/= pack "")
        |> drop 1
        |> map (\x -> read (unpack x) :: Int)

getDistanceForHoldPeriod :: Int -> Int -> Int
getDistanceForHoldPeriod maxTime holdTime = (maxTime - holdTime) * holdTime

getNumberOfWaysToBeat :: (Int, Int) -> Int
getNumberOfWaysToBeat (time, distance) =
  length waysToBeat
  where
    holdPeriods = [1 .. time - 1]
    distances = map (getDistanceForHoldPeriod time) holdPeriods
    waysToBeat = filter (> distance) distances

aoc6_1_Main :: IO ()
aoc6_1_Main = do
  input <- getContents
  print (mapTimeAndDistance input |> map getNumberOfWaysToBeat |> product)