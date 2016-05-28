import Data.List.Split

data Present = Present Int Int Int deriving (Show)

main = do
  contents <- readFile "input"

  print $ sum $ map calculateWrappingPaperSize $ map parsePresent $ lines contents

present [length, width, height] = Present length width height

parsePresent presentString = present $ map read $ splitOn "x" presentString

double x = x + x

calculateWrappingPaperSize (Present length width height) =
  sum(map double surfaces) + minimum(surfaces)

  where
    surfaces = [length * width, width * height, height * length]
