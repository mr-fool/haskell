reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:y) = reverse' y ++ [x]
