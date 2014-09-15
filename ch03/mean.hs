-- ch03/mean.hs

myMean :: [Double] -> Maybe Double
myMean [] = Nothing
myMean [a] = Just a
myMean xs = Just (sum xs / len xs)
    where
        sum [] = 0
        sum (x:xs) = x + sum xs
        len [] = 0
        len (x:xs) = 1 + len xs

-- better because list gets iterated onlye once (because sum_and_len gets evaluated only once
myMean' :: [Double] -> Maybe Double
myMean' [] = Nothing
myMean' [a] = Just a
myMean' xs = Just (fst (sum_and_len xs) / snd (sum_and_len xs))
    where
        sum_and_len [] = (0, 0)
        sum_and_len (x:xs) = (x + fst (sum_and_len xs), 1 + snd (sum_and_len xs))
