import Control.Monad
import Control.Monad.Cont (cont)
import Data.List (sort)
import System.IO

main = do
  contents <- readFile "input.txt"
  let vals = map readInt . words $ contents
  let evens = sort . evenElems $ vals
  let odds = sort . oddElems $ vals
  --   first problem
  --   let combined = sum [abs . pairDiff $ x | x <- zip evens odds]
  --   second problem
  let combined = sum [countOccurrences x odds * x | x <- evens]
  print combined

readInt :: String -> Int
readInt = read

evenElems :: [a] -> [a]
evenElems xs = [x | (x, i) <- zip xs [0 ..], even i]

oddElems :: [a] -> [a]
oddElems xs = [x | (x, i) <- zip xs [0 ..], odd i]

pairDiff :: (Num a) => (a, a) -> a
pairDiff (x, y) = x - y

countOccurrences :: (Eq a) => a -> [a] -> Int
countOccurrences x xs = length (filter (== x) xs)
