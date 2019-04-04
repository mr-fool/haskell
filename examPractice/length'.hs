length' :: [a] -> Int
length' [] = 0
length' (x:y) =  length' y +1
