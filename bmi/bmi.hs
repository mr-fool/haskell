bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal.. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some wieght, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ** 2
main = do
    putStrLn "Please enter your weight"
    weight <- getLine
    
    putStrLn "Please enter your height"
    height <- getLine    
    
    let numberWeight = read weight
    let numberHeight  = read height
    {-
    let bmiTell :: (RealFloat a) => a -> a -> String
        bmiTell weight height
            | bmi <= 18.5 = "You're underweight, you emo, you!"
            | bmi <= 25.0 = "You're supposedly normal.. Pffft, I bet you're ugly!"
            | bmi <= 30.0 = "You're fat! Lose some wieght, fatty!"
            | otherwise = "You're a whale, congratulations!"
            where bmi = weight / height ** 2 -}
    bmiTell numberWeight numberHeight

{-
ghc --make "bmi.hs" (in directory: /home/mr-fool/Documents/haskell/bmi)
[1 of 1] Compiling Main             ( bmi.hs, bmi.o )
bmi.hs:25:5:
    Couldn't match type ‘[]’ with ‘IO’
    Expected type: IO Char
      Actual type: String
    In a stmt of a 'do' block: bmiTell numberWeight numberHeight
    In the expression:
      do { putStrLn "Please enter your weight";
           weight <- getLine;
           putStrLn "Please enter your height";
           height <- getLine;
           .... }
Compilation failed.

-}
