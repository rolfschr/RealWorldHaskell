
intersperse :: Char -> [String] -> String
intersperse _ [] = ""
intersperse _ [x] = x
intersperse s (x:xs) = x ++ [s] ++ intersperse s xs
