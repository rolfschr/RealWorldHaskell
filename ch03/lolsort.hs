import Data.List
import Data.Ord

compByLength :: [a] -> [a] -> Ordering
compByLength xs ys
    | length xs > length ys = GT
    | length xs < length ys = LT
    | otherwise = EQ

sortByLenth :: [[a]] -> [[a]]
--sortByLenth xs = sortBy (comparing length) xs
sortByLenth xs = sortBy compByLength  xs
