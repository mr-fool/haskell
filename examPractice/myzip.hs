myzip :: [a] -> [b] -> [(a,b)]
myzip (x:y) (a:b) = (x,a) : zip y b
myzip _ _ = []
