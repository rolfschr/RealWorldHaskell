-- file: ch04/ch04.exercises.hs
import Data.Maybe
import Data.Char (digitToInt)

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [x] = Just []
safeInit (x:xs) = Just ([x] ++ (fromMaybe [] (safeInit xs)))
safeInit _ = Nothing

safeListFunc func [] = Nothing
safeListFunc func xs = Just (func xs)

safeHead' = safeListFunc head
safeTail' = safeListFunc tail
safeLast' = safeListFunc last
safeInit' = safeListFunc init

myF x = x == "_"

-- Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns False.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f xs = [pre] ++ (splitWith f suf)
    where
        (pre, suf) = break f (dropWhile f xs) -- use because break returns ([],rest) if seperator is the head

asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold (x:xs)
    | x == '-' = - asInt_fold xs
    | otherwise = foldl f 0 (x:xs)
    where
        f acc c = acc * 10 + digitToInt c
