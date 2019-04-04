main = do
    putStrLn "What year is it?"
    year	<-	getLine
    
    putStrLn "How old are you?"
    age		<-	getLine
    
    let numberYear = read year
    let numberAge  = read age
    
    putStrLn("Ah! Then you must be born in " ++ show( (numberYear - numberAge) ) )
