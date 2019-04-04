module Main where
import Data.Char


main = do
	putStrLn "Guess a female name that I'm thinking of ..."
	guess <- getLine
	
	--changing the input to uppercase for comparision
	let uppercase :: String -> String
	    uppercase = map toUpper
	
	let uppercaseString = uppercase guess
	case uppercaseString of 
		"CORTANA"	-> 	putStrLn "Nerd"
		_			->	putStrLn "Tough luck"
