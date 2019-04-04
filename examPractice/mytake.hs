mytake :: Int -> [a] -> [a]
mytake 0 a = []
mytake n [] = []
mytake n (x:xs) = x : mytake (n-1) xs
