main = do
    putStrLn "Please enter the amount of USD you wish to convert to EURO"
    usd <- getLine
    let dollar = read usd :: Double
    let currencyConvert us = us * 0.76
    putStrLn ("The amount of EURO is " ++ show(currencyConvert dollar) )
