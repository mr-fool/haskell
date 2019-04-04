main = do
    putStrLn "Please enter the current hour."
    hour <- getLine
    
    case read hour of
        11	->	putStrLn "Aren't you getting hungry?"
        16	->	putStrLn "You'll be getting off of work soon!"
        22	-> 	putStrLn "You certainly have to be tired now."
        _	-> putStrLn "Alright."
        
