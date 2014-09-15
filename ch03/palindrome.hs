-- ch03/palindrome.hs

makePalindrome :: [a] -> [a]
makePalindrome [] = []
makePalindrome (x:xs) = [x] ++ (makePalindrome xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (x:xs)
    | x == last xs = isPalindrome (init xs)
    | otherwise = False

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [a] = True
isPalindrome' xs = xs == reverse xs
