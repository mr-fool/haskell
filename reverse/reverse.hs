import Data.List
import Text.Read
myLast = reverse
main = do 
	putStrLn "Please enter a list"
	a <- getLine
	putStrLn "Reversing your list"
	putStrLn (myLast a)

