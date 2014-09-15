-- ch03/palindrome.hs

makePalindrome :: [a] -> [a]
makePalindrome [] = []
makePalindrome (x:xs) = [x] ++ (makePalindrome xs) ++ [x]
