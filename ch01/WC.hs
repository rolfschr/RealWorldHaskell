
main = interact output
	where
		output input = show (linecount input) ++ " " ++ show (wordcount input)  ++ " " ++ show (bytecount input) ++ "\n"
		wordcount input = length (words input)
		linecount input = length (lines input)
		bytecount input = length input
