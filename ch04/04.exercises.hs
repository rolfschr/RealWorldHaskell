-- file: ch04/ch04.exercises.hs
import Data.Maybe
import Data.Char (digitToInt)
import Data.Char (isSpace)

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

concat_fold :: [[a]] -> [a]
concat_fold xs = foldr (++) [] xs

test_concat_fold = concat_fold [[1], [2,3], [4]]

takeWhile_expl :: (a -> Bool) -> [a] -> [a]
takeWhile_expl p [] = []
takeWhile_expl p (x:xs)
    | p x = x : takeWhile_expl p xs
    | otherwise = []

test_takeWhile_expl = takeWhile_expl (odd) [1,3,5,2,4,7]

-- If we do expression substitution until the end of the list (assuming a finite list), a foldl will end up with the first element in the innermost expression and the last element in the outermost expression. So a strict foldl will start evaluating a foldl with the first element, whereas a lazy foldl will start with the last element.
--
-- In the case of foldr, it's the other way round. Therefore a lazy foldr will start with evaluating the expression for the first element, and can handle infinite lists, because it doesn't need to look at any elements after the last one it needs for its result.
--
-- Or in another perspective, with an example:
--
-- takeWhile_foldr p = foldr f [] where f x xs = if p x then x : xs else []
--
-- So this is my take at a solution for this exercise. Important here is the variable xs to the helper function f. This xs is actually a thunk representing the recursive calls foldr does on the following elements of the list. When f returns [], this xs remains unused and will therefore never be forced to evaluate.
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
takeWhile_fold :: (a -> Bool) -> [a] -> [a]
takeWhile_fold p xs = foldr step [] xs
    where
        step x ys
            | p x = x : ys
            | otherwise = []

test_takeWhile_fold = takeWhile_fold (odd) [1,3,5,2,4,7]

groupBy :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
--groupBy f [x] = [[x]]
groupBy f (x:xs) = foldr step [[]] (x:xs)
    where
        step x (y:ys)
            | null y = [[x]]
            | x `f` (head y) = (x:y) : ys
            | otherwise = [x] : y : ys

test_groupBy = groupBy (==) "MAssissippZ"
test_groupBy2 = groupBy (<) [1,2,2,4,4,5,6,7,8,9,9]

any_fold :: (a -> Bool) -> [a] -> Bool
any_fold p xs = foldr f False xs
    where
        f x b = b || p x

any_fold' :: (a -> Bool) -> [a] -> Bool
any_fold' p = foldr (\x b -> p x || b) False

test_any_fold = any_fold (>9) [1,2,3,4,5,6,11]

cycle_fold :: [a] -> [a]
cycle_fold xs = foldr f [] [1..]
    where
        f _ ys = xs ++ ys

words_fold :: String -> [String]
words_fold s = foldr f [[]] s
    where
        f c (y:ys)
            | c == ' ' = [] : y : ys
            | otherwise =  (c:y) : ys

words_fold' :: String -> [String]
words_fold' s = groupBy (\c1 c2 -> c1 /= ' ') s

unlines_fold :: [String] -> String
unlines_fold ss = foldr1 f ss
    where
        f s ys = s ++ "\n" ++ ys

test_unlines_fold = unlines_fold ["fst line", "scond", "thrid"]
