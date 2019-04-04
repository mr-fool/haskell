import System.Random

main = do
  let randomize = randomRIO (0, 6) :: IO Int
  r <- randomize
  print r
  r <- randomize
  print r
