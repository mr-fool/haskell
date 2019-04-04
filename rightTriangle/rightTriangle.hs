main = do {
	--taking sides input
	
	; putStrLn "Please enter the length of a"
	; a <- readLn
	
	; putStrLn "Please enter the length of b"
	; b <- readLn
	
	; putStrLn "Please enter the length of c"
	; c <- readLn
	
	; let checking :: Int -> Int -> Int -> IO ()
	      checking a b c =
		if (a^2 + b^2 == c^2 && c >= 1 && b >= 1 && b <= c && a >=1 && a <=b)
			then putStrLn ("it is a right triangle")
			else do putStrLn ("it is not a right triangle")
        ; checking a b c
 }
