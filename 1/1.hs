main = do
  contents <- readFile "input"

  print $ floorsTravelledUntilBasementEntered contents

floorsTravelledUntilBasementEntered movements =
  length
  $ takeWhile aboveBasement
  $ traverseLevels
  $ map parseChar movements

aboveBasement level = 0 <= level

traverseLevels levels = scanl (+) 0 levels

parseChar char =
  case char of
    '(' -> 1
    ')' -> -1
