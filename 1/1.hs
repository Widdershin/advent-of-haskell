main = do
  contents <- readFile "input"
  print $ sum $ map parseChar contents

parseChar char =
  case char of
    '(' -> 1
    ')' -> -1
    _ -> 0
