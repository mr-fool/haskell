main = do
  print "Please enter a number"
  a <-readLn
  print (factorial a)

factorial :: Integer -> Integer
factorial n = product [1..n]             
