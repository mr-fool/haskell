main = do
    putStrLn "Please enter the temperature in celsius"
    temperature <- getLine
    let celsius = read temperature :: Double
    let cels2fahr a = a * 1.8 +32 
    let result = show (cels2fahr celsius)
    putStrLn ("The temperature in fahrenheit " ++ result)
  
