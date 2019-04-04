main = do 
	putStrLn "What is your first name?"
	first_name <- getLine
	
	putStrLn "And your last name?"
	last_name <- getLine
	
	putStrLn "What is your street address?"
	address <- getLine
	
	putStrLn "May I have your postal code please?"
	postal_code <- getLine
	
	putStrLn "And the postal town?"
	town <- getLine
	
	putStrLn "On envelopes, write:\n"
	putStrLn (first_name ++ " " ++ last_name)
	putStrLn address
	putStrLn (postal_code ++ " " ++ town)
	
	
	
