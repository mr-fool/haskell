import Data.Char

main = do
    putStrLn "Please enter your user name"
    userName	<-	getLine
    
    putStrLn "Please enter your password"
    password	<-	getLine
    
    let uppercase :: String -> String
        uppercase = map toUpper
    
    let uppercaseString = uppercase userName
    case uppercaseString of 
        "JOHN"	->	case password of 
            "poodle23"	-> putStrLn "Authenticating...\nSuccessful"
            _	-> putStrLn "Wrong password!"
        _	-> putStrLn "Wrong username!"
    putStrLn "End of transmission."
    
            
