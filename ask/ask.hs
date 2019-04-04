import Data.Char

main = do
    putStrLn "Where do you live?"
    place <- getLine
    
    let uppercase :: String -> String
        uppercase = map toUpper
    
    let uppercaseString = uppercase place
    case uppercaseString of 
        "Calgary"	->	putStrLn "Oh no, not Calgary!"
        _			->	putStrLn (place ++ ", huh? Great place to live.")
         
