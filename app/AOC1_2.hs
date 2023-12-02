module AOC1_2 (getSumFromDocument) where

import Data.Char (isDigit)
import Data.Semigroup ((<>))
import Data.Text (Text, find, findIndex, isPrefixOf, pack, replace, reverse, singleton, splitOn, unpack)
import Data.Traversable (for)
import Flow ((|>))

toDigit :: String -> String
toDigit l@('o' : 'n' : 'e' : _) = '1' : (toDigit . tail $ l)
toDigit l@('t' : 'w' : 'o' : _) = '2' : (toDigit . tail $ l)
toDigit l@('t' : 'h' : 'r' : 'e' : 'e' : _) = '3' : (toDigit . tail $ l)
toDigit l@('f' : 'o' : 'u' : 'r' : _) = '4' : (toDigit . tail $ l)
toDigit l@('f' : 'i' : 'v' : 'e' : _) = '5' : (toDigit . tail $ l)
toDigit l@('s' : 'i' : 'x' : _) = '6' : (toDigit . tail $ l)
toDigit l@('s' : 'e' : 'v' : 'e' : 'n' : _) = '7' : (toDigit . tail $ l)
toDigit l@('e' : 'i' : 'g' : 'h' : 't' : _) = '8' : (toDigit . tail $ l)
toDigit l@('n' : 'i' : 'n' : 'e' : _) = '9' : (toDigit . tail $ l)
toDigit (d : r) = d : toDigit r
toDigit [] = []

extractNumber :: Text -> Int
extractNumber line =
  read (firstNumber <> secondNumber) :: Int
  where
    firstNumber = case find isDigit line of
      Just maybeNumber -> unpack (singleton maybeNumber)
      Nothing -> ""
    secondNumber = case find isDigit (Data.Text.reverse line) of
      Just maybeNumber -> unpack (singleton maybeNumber)
      Nothing -> ""

getSumFromDocument :: String -> Int
getSumFromDocument document =
  splitOn (pack "\n") (pack document)
    |> map unpack
    |> filter (/= "")
    |> map toDigit
    |> map pack
    |> map extractNumber
    |> sum

aoc1_2_Main :: IO ()
aoc1_2_Main = do
  document <- getContents
  print (getSumFromDocument document)
