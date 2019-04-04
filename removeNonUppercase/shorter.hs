import Data.Char
main = do
	--taking user input
	putStrLn "Please enter a String that you want to remove lowercase"
	input <- getLine
	putStrLn (removeNonUppercase input)

removeNonUppercase s = filter isUpper s
