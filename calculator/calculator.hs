main = do
    putStrLn "Do you wnat to (a)dd, (s)ubtract or (m)ultiply?"
    operation	<-	getLine
    
    putStrLn "Enter the first number."
    num1	<-	getLine
    
    putStrLn "Enter the second number."
    num2	<-	getLine
	
    let newNum1 = read num1 :: Int
    let newNum2 = read num2 :: Int
    let operator = case operation of
            "a" -> (+)
            "s" -> (-)
            "m" -> (*)
        
    let answer =  newNum1 `operator` newNum2
    putStrLn("The result is " ++ show answer)

