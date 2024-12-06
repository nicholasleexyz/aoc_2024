module Main where

import Data.List
import Data.List.Split
import Text.Regex.TDFA

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ sum $ caclPairProducts $ convertToInts $ removeBeforeDoCalls $ filterForDoCalls $ splitOnDont $ removeNoise $ "do()" ++ contents

-- remove noise
removeNoise :: String -> String
removeNoise str =
  let pattern = "do\\(\\)|don't\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
   in concat (getAllTextMatches (str =~ pattern) :: [String])

splitOnDont :: String -> [String]
splitOnDont = splitOn "don't()"

doCallCheck :: String -> Bool
doCallCheck str = "do()" `isInfixOf` str

filterForDoCalls :: [String] -> [String]
filterForDoCalls = filter doCallCheck

removeBeforeDoCalls :: [String] -> [String]
removeBeforeDoCalls = map (drop 4 . removeBeforeSubstring "do()")

convertToInts :: [String] -> [Int]
convertToInts = concatMap findNumbers

removeBeforeSubstring :: String -> String -> String
removeBeforeSubstring subString str
  | subString `isInfixOf` str = drop (length prefix) str
  | otherwise = str
  where
    prefix = takeWhile (/= head subString) str

findNumbers :: String -> [Int]
findNumbers str =
  let pattern = "[0-9]{1,3}"
   in map (\s -> read s :: Int) (getAllTextMatches (str =~ pattern) :: [String])

caclPairProducts :: [Int] -> [Int]
caclPairProducts ns = zipWith (*) (everyOther ns) (everyOther (drop 1 ns))

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x : xs) = x : everyOther (drop 1 xs)
