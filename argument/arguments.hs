import System.Environment

main = do
    args	<-	getArgs
    --putStrLn ("You gave " ++ show (length args) ++ " arguments")
    
    let inputChecking a =
	    if length args /= 2
	        then putStrLn "The number of arguments are incorrect"
	    else 
	        putStrLn "The correct number of args"
	        
    inputChecking args   
    putStrLn ("You gave " ++ show (length args) ++ " arguments")
	    
