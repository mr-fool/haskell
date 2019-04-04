main = do
    putStrLn "Enter a name"
    name_1 <- getLine
    putStrLn "Enter another name, please"
    name_2 <- getLine

    case name_1 < name_2 of
        True	-> putStrLn (name_1 ++ " comes before " ++ name_2 ++ " in the phone book!")
        False	-> putStrLn (name_1 ++ " comes after " ++ name_2 ++ " in the phone book!")

