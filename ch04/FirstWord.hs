
import System.Environment (getArgs)

interactWith :: (String -> String) -> String -> String -> IO()
interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main :: IO()
main = mainWith firstWords
    where
        mainWith function = do
            args <- getArgs
            case args of
                [input,output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"

firstWords :: String -> String
firstWords input = concatMap  (\x -> (head' (words x)) ++ "\n") (lines input)

head' :: [[Char]] -> [Char]
head' (x:_) = x
head' _ = ""
