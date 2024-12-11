import Data.List

main :: IO()
main = do
  input <- readFile "input.txt"
  print $ sum $ map countOccurences $ allDirections $ lines input

type Matrix a = [[a]]
type Line a = [a]

diagonals :: Matrix a -> [Line a]
diagonals orig = concat [
      transpose $ zipWith drop [0 ..] orig
    , transpose $ map reverse $ zipWith take [0 ..] orig
    ]

allDirections :: Matrix a -> [Line a]
allDirections orig = concat [
      orig                             -- horizontals
    , transpose orig                   -- verticals
    , diagonals orig                   -- diagonals (sloping down)
    , diagonals (map reverse orig)     -- diagonals (sloping up)
  ]

countOccurences :: String -> Int
countOccurences str = countOccurences' str 0

countOccurences' :: String -> Int -> Int
countOccurences' [] acc = acc
countOccurences' str acc
  | xmasCheck (take 4 str) = countOccurences' (drop 1 str) (acc + 1)
  | otherwise = countOccurences' (drop 1 str) acc

xmasCheck :: String -> Bool
xmasCheck str = (str == "XMAS") || (str == "SAMX")
