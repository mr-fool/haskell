mylast' :: [a] -> a
mylast' [] = error "empty list not cool"
mylast' (x:[]) = x
mylast' (x:y) = mylast' y
