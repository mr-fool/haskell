main = do
	--taking user input
	putStrLn "Please enter a String that you want to remove lowercase"
	input <- getLine
	putStrLn (removeNonUppercase input)
	
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
