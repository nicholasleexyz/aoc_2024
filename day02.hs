import Data.List (nub)

main :: IO()
main = do
  contents <- readFile "input.txt"
  let lineLst = lines contents
  let numLstLst = map (map (read :: String -> Int) . words) lineLst
  let safeNumLstLst = filter isSafeTolerant numLstLst

  print (length $ filter safeCheck numLstLst)
  print (length safeNumLstLst)

safeCheck :: [Int] -> Bool
safeCheck report =
    let ds = deltas report
    in (length (nub (map (signum) ds)) == 1)
      && (all (\d -> abs d `elem` [1..3]) ds)

deltas :: (Num a) => [a] -> [a]
deltas xs = zipWith (-) (tail xs) xs

stalinSort :: (Ord a) => [a] -> [a]
stalinSort = foldl (\acc x -> acc ++ [x | null acc || x > last acc]) []

isSafeTolerant :: (Num a, Ord a) => [a] -> Bool
isSafeTolerant xs =
  let sorted = stalinSort (if head xs > last xs then reverse xs else xs)
      absDeltas = map abs (deltas sorted)
      filteredDeltas = filter (<= 3) absDeltas
      lenFiltered = length filteredDeltas
   in lenFiltered == length xs - 1 || lenFiltered == length xs - 2

