import Data.Char

main = do
    putStrLn "Please remember username and password are case sensitve"
    putStrLn "Username:"
    username <- getLine
    putStrLn "Password:"
    password <-getLine
    putStrLn "Authemticating..."
    let trim :: String -> String
        trim = f . f
            where f = reverse . dropWhile isSpace
    let trimUsername = trim username
    let trimPassword = trim password
    
    case trimUsername ++ "/" ++ trimPassword of 
        "steve/123" -> do
            putStrLn "Username and password correct."
            putStrLn "Secret: Moondust collected!"
        _ -> putStrLn "Inccorect credentials"
    putStrLn "End of transmission"
    
       
