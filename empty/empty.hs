main = do
    putStrLn "Just click enter"
    empty1 <- getLine
    
    putStrLn "Just click enter"
    empty2 <- getLine
    
    putStrLn "Just click enter"
    empty3 <- getLine
    
    let newEmpty = empty1 ++ empty2 ++ empty3
    case newEmpty of 
        ""	->	putStrLn "All three input is empty"
        _	->	putStrLn "The inputs are not empty"
            

    
    
    
