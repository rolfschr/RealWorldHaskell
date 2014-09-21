-- file: ch04/ch04.exercises.hs
import Data.Maybe

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
