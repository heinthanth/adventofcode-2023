module AOC6_2 where

import Data.Text (pack, splitOn, unpack)
import Debug.Trace (traceShow)
import Flow ((|>))

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
  t2 - t1 - 1
  where
    delta = time ^ 2 - 4 * distance
    rdelta = sqrt (fromIntegral delta :: Float)
    t1 = floor (((fromIntegral time :: Float) - rdelta) / 2.0)
    t2 = ceiling (((fromIntegral time :: Float) + rdelta) / 2)

aoc6_2_Main :: IO ()
aoc6_2_Main = do
  input <- getContents
  print (mapTimeAndDistance input |> map getNumberOfWaysToBeat |> head)