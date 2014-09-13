-- ch02/lastButOne.hs
lastButOne :: [a] -> Maybe a
--lastButOne xs = head (drop (length xs - 2) xs)
lastButOne xs = if length xs > 2
                then lastButOne (tail xs)
                else if length xs == 2
                    then Just (head xs)
                    else Nothing
