import System.Environment (getArgs)

interactWith :: (String -> String) -> String -> String -> IO()
interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main :: IO()
main = mainWith t
    where
        mainWith function = do
            args <- getArgs
            case args of
                [input,output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"

t :: String -> String
t input = concat (transpose (lines input))

hw = "hello\nworld\n"
hwd = "hello\nworldd\n"

transpose :: [[a]] -> [[a]]
transpose xs
    | any null xs = []
    | otherwise = (map head xs) : transpose (map tail xs) 

transpose' :: [String] -> [String]
transpose' xs
    |all null xs = []
    | otherwise = heads : transpose' tails
        where
            heads = concat (map safeHead xs)
            tails = map (drop 1) xs
            safeHead "" = " "
            safeHead s = take 1 s

transpose'' :: [[a]] ->[[a]]
transpose'' [] = repeat []
transpose'' (xs : xss ) = zipWith (:) xs (transpose'' xss)
