
module Othello where
import OthelloTools
import System.Environment
import System.Exit


---------------------From Othello.hs------------------------------



{- | This is the type for all player functions.  A player strategy function takes a
board and returns a point (Int,Int) that it chooses -- this should be a legal move.
If the player passes, the funciton should return Nothing.
-}
type Chooser = GameState -> Cell -> [Maybe (Int,Int)]


-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                    in  (if null zs then (if null ys then [] else init ys) else ys) ++ [elem] ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

---------------------Other Choosers-------------------------------

-----------------GREEDY--------------
-- | This is a sample greedy strategy
greedy :: Chooser
greedy (GameState {play = p, theBoard = b}) c = mapJust (maxCaptures (findAllMovesAndCaptures b c))

-- | helper function for greedy chooser, takes a list of valid moves and returns the one with the
--  most caputures
maxCaptures :: [[(Int,Int)]] -- ^ Takes in the list of valid moves in the form of a nested list of Int pairs
			-> [(Int, Int)] -- ^ Returns the move with the most captures along with the captured pieces
maxCaptures moves = foldr findMax [] moves

-- | helper function for greedy chooser, compares two moves and determines which will capture more
findMax :: [(Int,Int)]  -- ^ First move to be compared
        -> [(Int,Int)]  -- ^ Second move to be compared
        -> [(Int,Int)]  -- ^ Returns the move with most captures

findMax [] [] = []
findMax [] a = a
findMax a b
 | length a > length b = a
 | otherwise = b

-- | since choosers retrun Maybe (Int, Int)s this makes a move into a 'Just' move
mapJust :: [(Int, Int)] -- ^ Takes in a normal list of Int pairs
		-> [Maybe (Int, Int)] -- ^ Returns a casted Just pair of Ints
mapJust [] = []
mapJust (x:xs) = Just (fst x, snd x) : mapJust xs
--------------------------------------



-------------------FirstAvailable----------------------------------------
-- | A strategy that chooses the first of all available moves
firstAvailable_st :: Chooser
firstAvailable_st (GameState {play = p , theBoard =b})  c = mapJust(firstAvailable (findAllMovesAndCaptures b c))

-- | Helper function for the firstAvailable_st strategy
firstAvailable :: [[(Int,Int)]] -> ([(Int,Int)])
firstAvailable moves
  | moves == [] = []
  | otherwise = (head (moves))
-------------------------------------------------------------------------


------------------corner--------------------------------------------------
-- | Strategy which chooses moves nearest to the corners of the board
corner_st :: Chooser
corner_st (GameState {play = p , theBoard =b})  c = mapJust( corner (findAllMovesAndCaptures b c))
-- | Helper function for corner strategy
mysubtract_1 :: (Int,Int) -> (Int,Int)
mysubtract_1  (a,b) = (a-1,b-1) 
-- | Helper function for corner strategy
mysubtract_2 :: (Int,Int) -> (Int, Int) 
mysubtract_2  (a,b) = (a-1,b-8)
-- | Helper function for corner strategy
mysubtract_3 :: (Int,Int) -> (Int, Int) 
mysubtract_3  (a,b) = (a-8,b-1)
-- | Helper function for corner strategy
mysubtract_4 :: (Int,Int) -> (Int, Int) 
mysubtract_4  (a,b) = (a-8,b-8)
-- | Helper function for corner strategy
mysquare :: (Int,Int) -> Int 
mysquare (a,b) = a^2+b^2 

{- | get 4 distances between cell postion and 4 corner position(1,1)(1,8)(8,1)(8,8)-}
getAllDistance :: [(Int,Int)] -> [Int] 
getAllDistance a = [mysquare(mysubtract_1(head(a))),mysquare(mysubtract_2(head(a))),mysquare(mysubtract_3(head(a))),mysquare(mysubtract_4(head(a)))] 

{- | find smallest distance among above 4 distances-}
findSmall :: [Int] -> Int
findSmall x = minimum x

{- | make array which contain smallest distance and list-}
makeArr :: [(Int,Int)] -> (Int,[(Int,Int)])
makeArr x = (findSmall(getAllDistance x) , x )

{- | find all lists' smallest distance and compare which distance is smallest-}
mycalculator :: [(Int, Int)] -> (Int,[(Int,Int)]) -> (Int,[(Int,Int)])
mycalculator moves ((-1),[ ]) = makeArr moves
mycalculator moves (s,x) = if(fst(makeArr moves)>=s) then (s,x) else makeArr moves

{- | find all lists' smallest distance and compare which distance is smallest-}
corner :: [[(Int,Int)]] -> ([(Int,Int)])
corner x = snd(foldr mycalculator ((-1),[ ]) x)


--------------------------------------------------------------------------

----------------Strategy Names and Functions----------------------

-- | these are the valid strategies
data Strategy = First | Greedy | Corner | DoesNotExist

instance Show (Strategy) where
  show s = strategy2Strn s

instance Eq (Strategy) where
  s1 == s2 = strategy2Strn s1 == strategy2Strn s2

-- | Converts a Strategy data type to a string representation
strategy2Strn   :: Strategy -> String
strategy2Strn First = "First"
strategy2Strn Greedy = "Greedy"
strategy2Strn Corner = "Corner"
strategy2Strn DoesNotExist = "This is not a strategy"

-- | converts a string into a strategy
strn2Strategy   :: String -> Strategy
strn2Strategy "Corner" = Corner
strn2Strategy "Greedy" = Greedy
strn2Strategy "First" = First
strn2Strategy _ = DoesNotExist

-- | converts strategies into their cooresponding chooser types
strategy2Chooser :: Strategy -> Chooser
strategy2Chooser Greedy = greedy
strategy2Chooser First = firstAvailable_st
strategy2Chooser Corner = corner_st

-- | composses strn2Strategy and strategy2Chooser
strn2Chooser :: String -> Chooser
strn2Chooser s = strategy2Chooser (strn2Strategy s)

-- | Prints out the strategy
putStrategy     :: Strategy -> IO()
putStrategy s = putStr (strategy2Strn s)

---------------------Playing the game functions--------------------
----------------------Don't put IO in these------------------------

-- | Have to convert between the coordinates returned by findMovesAndCaptures in order to use the rep
convert :: (Int,Int) -- ^ Takes in a pair which represents the initial cooridnates
        -> (Int,Int) -- ^ Returns the converted pair
convert x = (((fst x) - 1), (abs ((snd x) - 8)))

-- | this will palce an individual piece on a board
placePiece :: (Int, Int)  -- ^ Place to put the piece (in Std coordinates)
           -> Board       -- ^ Current board to place Piece
           -> Cell        -- ^ Represents which player is placing the piece
           -> Board       -- ^ Returns the board with piece played
placePiece mv b p = replace2 b ((fst mv - 1), (8 - snd mv)) p

-- | this will take in an entire move and play/capture each piece
playMove :: [Maybe (Int, Int)] -- ^ The move from a chooser
         -> Board              -- ^ The board to play the move on
         -> Cell               -- ^ The Player making the move
         -> Board              -- ^ Returns the board after the move is made
playMove [] b _ = b
playMove (mv:mvs) b p
 | mv == Nothing = playMove mvs b p
 | otherwise = placePiece (maybe (0,0) (\x -> x) mv) (playMove mvs b p) p

-- | Converts a player Type into is cooresponding Cell type
playedBy :: Player -- ^ Takes in the player that is playing
			-> Cell -- ^ Returns the corresponding representation of the player
playedBy White = W
playedBy Black = B

-- | This function is used to create the 'Played' variable in a new gamestate.
--   It will take in a move from a chooser and determine if it is a pass or where
--   the chooser chose to play
newPlayedFrom :: [Maybe (Int, Int)] -- ^ Takes in a pair of Maybe Ints
				-> Played -- ^ Returns whether or not the move is a pass or play
newPlayedFrom [] = Passed
newPlayedFrom moves = Played (convert ((maybe (0,0) (\x -> x) (head moves))))


-- | Since moves are of type [Maybe (Int, Int)] we need sometimes need to remove the
--   'maybes'. This function does that.
unMaybe :: [Maybe (Int, Int)] -- ^ Takes in a Maybe pair of Ints
		-> [(Int, Int)] -- ^ Returns a normal pair of Ints
unMaybe = map unmaybe
  where unmaybe :: Maybe (Int, Int) -> (Int, Int)
        unmaybe = maybe (0, 0) (\y -> y)

-- | next gamestate represnts a chooser taking a turn in the game
nextGamestate :: Chooser     -- ^ The chooser taking a turn
              -> GameState   -- ^ The gamestate before the turn is taken
              -> GameState   -- ^ Returns the gamestate after the turn is taken
nextGamestate c (GameState {play = p, theBoard = b}) =
  GameState (player, (newPlayedFrom setOfMoves)) (playMove setOfMoves b playerCell)

  where
    player = invertPlayer (fst p)
    playerCell = playedBy player
    setOfMoves = c (GameState p b) playerCell


-- | since the board starts with a play variable (Black, Init) we need a
--   different function to make the first move that will not flip the Player
--   variable within Play
firstMove :: Chooser -- ^ Takes in the strategy of the first player
		-> GameState -- ^ Takes in the initial gamestate
		-> GameState -- ^ Returns the new gamestate after the move is played
firstMove c (GameState {play = p, theBoard = b}) =
  GameState (player, (newPlayedFrom setOfMoves)) (playMove setOfMoves b playerCell)

  where
    player = fst p
    playerCell = playedBy player
    setOfMoves = c (GameState p b) playerCell

-- | endgame will determine the winner and print out an endgame message
endgame :: String -> String -> Board -> IO ()
endgame c1 c2 b
  | countBlackPieces b > countWhitePieces b = putStrLn ("Black wins! Black (" ++ c1 ++ ") : " ++ show (countBlackPieces b) ++  " White (" ++ c2 ++ ") :" ++ show (countWhitePieces b))
  | countBlackPieces b == countWhitePieces b = putStrLn ("Tied!, Black (" ++ c1 ++ ") : " ++ show (countBlackPieces b) ++  " White (" ++ c2 ++ ") :" ++ show (countWhitePieces b))
  | otherwise                       = putStrLn ("White wins! Black (" ++ c1 ++ ") : " ++ show (countBlackPieces b) ++  " White (" ++ c2 ++ ") :" ++ show (countWhitePieces b))

-- | counts the black pieces on a board
countBlackPieces :: Board -> Int
countBlackPieces [] = 0
countBlackPieces (x:xs) = (countBlack x) + (countBlackPieces xs)
  where countBlack :: [Cell] -> Int
        countBlack [] = 0
        countBlack (x:xs)
         | x == B = 1 + countBlack xs
         | otherwise = 0 + countBlack xs

-- | counts the white pieces on a board
countWhitePieces :: Board -> Int
countWhitePieces b = countBlackPieces (invertBoardPieces b)

-----------------------------main----------------------------------
-- | The main function which outputs intermediate boards 
playTheGame :: Strategy -> Strategy -> GameState -> IO ()
playTheGame active inactive (GameState {play = p, theBoard = b})
      | (lastPlay == Passed) && (newSetOfMoves == []) = do
           if (lastPlayer == Black)
             then endgame strat2String strat1String b
           else endgame strat1String strat2String b
      | (lastPlay `elem` [Goofed (a, b) | a <- [0..8], b <- [0..8]]) = do
        print currentGameState
        putStrLn "Game Over"
        if (lastPlayer == Black)
          then endgame strat2String strat1String b
        else endgame strat1String strat2String b
      | (lastPlay == IllegalPass) = do
        print currentGameState
        putStrLn "Illegal pass, Game Over"
        if (lastPlayer == Black)
          then endgame strat2String strat1String b
        else endgame strat1String strat2String b
        
      | otherwise = do
        print newGameState
        playTheGame inactive active newGameState

        where
        lastPlayer = fst p
        lastPlay = snd p
        currentGameState = GameState p b
        currentPlayer = invertPlayer (fst p)
        newSetOfMoves = (strategy2Chooser active) (GameState p b) (playedBy currentPlayer)
        newMove = head (unMaybe newSetOfMoves)
        newGameState = nextGamestate (strategy2Chooser active) currentGameState
        strat1String = strategy2Strn active
        strat2String = strategy2Strn inactive

-- | this is the main function
main = do
    argument	<-	getArgs

    putStrLn "Hello, Welcome to the CPSC449 Othello Assignment"
    putStrLn "Valid Strategies are:"
    putStrLn "  Greedy"
    putStrLn "  First"
    putStrLn "  Corner"

    let inputChecking a =
		if (strn2Strategy a /= DoesNotExist)
			then putStr ("valid Strategy " ++ a ++ " selected\n")
		else do
            putStr "invalid Strategy\n"
            putStrLn "Valid Strategies are:"
            putStrLn "  Greedy"
            putStrLn "  First"
            putStrLn "  Corner"
            exitSuccess

    if length argument == 2
		then do
            let s1 = (head argument)
            let s2 = (argument !! 1)
            inputChecking s1
            inputChecking s2
            print initBoard
            print (firstMove (strn2Chooser s1) initBoard)
            playTheGame (strn2Strategy s2) (strn2Strategy s1) (firstMove (strn2Chooser s1) initBoard)
    else
		if length argument == 0
			then do

                putStrLn "Please select a black strategy"
                s1 <- getLine
                inputChecking s1
                putStrLn "Please select a white strategy"
                s2 <- getLine
                inputChecking s2
                print initBoard
                print (firstMove (strn2Chooser s1) initBoard)
                playTheGame (strn2Strategy s2) (strn2Strategy s1) (firstMove (strn2Chooser s1) initBoard)
		else
			do
                putStrLn "Invalid number of arguments"
                putStrLn "Valid Strategies are:"
                putStrLn "  Greedy"
                putStrLn "  First"
                putStrLn "  Corner"
                exitSuccess



------------------------Running FSM on a board-----------------------------



-- | findMoves takes a board and returns only the squares to place pieces
--   This is currently not being used by the main program
findMoves :: Board          -- ^ The board to find moves on
          -> [(Int, Int)]   -- ^ Returns a list of valid squares to move on
findMoves [] = []
findMoves x = map head (findMovesAndCaptures' x)


-- | findMovesAndCaptures take a board and cell and returns each valid move
--   along with the cells that will be captured if that move is played
findMovesAndCaptures :: Board          -- ^ The board to find moves on
                     -> Cell           -- ^ The player cell whose moves will be found
                     -> [[(Int, Int)]] -- ^ Returns the list of moves and captures
findMovesAndCaptures b cell
  | cell == W = findMovesAndCaptures' (invertBoardPieces b)
  | otherwise = findMovesAndCaptures' b


-- | findMovesAndCaptures' is a helper function for findMovesAndCaptures
--   it returns all valid moves and captures for the black pieces on a board
findMovesAndCaptures' :: Board          -- ^ The board to find moves on
                      -> [[(Int, Int)]] -- ^ Returns the list of moves and captures
findMovesAndCaptures' [] = []
findMovesAndCaptures' (x:xs) = movesAndCapturesOnRow (succ (length xs)) (runFSML x) ++ findMovesAndCaptures' xs


-- | findAllMovesAndCaptures returns all valid moves on the board as a double nested list where the "lowest" list
--   contains a move and a list of pieces which that move will capture e.g. [[move, captures ...]]

findAllMovesAndCaptures :: Board -- ^ The board to find moves on
                        -> Cell  -- ^ The cell of the player whose moves we are searching for
			-> [[(Int, Int)]] -- ^ Returns the list of moves and captures
findAllMovesAndCaptures b c
  | c == W = compress (findAllMovesAndCaptures' (invertBoardPieces b))
  | otherwise = compress (findAllMovesAndCaptures' b)
     where compress :: [[(Int,Int)]] -> [[(Int,Int)]]
           compress a = compressAllMovesAndCaptures (compressAllMovesAndCaptures a)

-- | findAllMovesAndCaptures' will assume that we are looking for black pieces and find all moves and
--   captures for the black player.
findAllMovesAndCaptures' :: Board -- ^ The board to find moves on
                         -> [[(Int, Int)]] -- ^ Returns the list of moves and captures
findAllMovesAndCaptures' board = [elem| perm<-[ map(map (`mapMoves` r))(findMovesAndCaptures' (rotateX board r )) | r  <- [0,1,2,3]], elem<-perm]

-- | This is currently not used in any other functions or the execution of the main program
--   This might be usefull in speeding things up if you make it without making calls to movesandcaptures
validMovesOnRow :: [[(Int, Int)]] -- ^ Takes in a nested list of coordinates representing the board
				 -> [(Int,Int)] -- ^ Returns a list of valid moves in a row
validMovesOnRow [] = []
validMovesOnRow (x:xs) = head x : validMovesOnRow xs

-- | This is a helper function for findMoves and Captures, it uses the info from the FSM to create valid (Int, Int) pairs
--   returns the valid moves
movesAndCapturesOnRow :: Int               -- ^ The value of the row fed to the fucntion. Rows are counted upwards from the bottom of a board
                      -> [(Int, Int, Int)] -- ^ The memory from the FSM on the row above
                      -> [[(Int, Int)]]    -- ^ Returns a list of valid moves and the spaces they will capture
movesAndCapturesOnRow _ [] = []
movesAndCapturesOnRow x (y:ys) = (makeSetofCaptures x y True) : movesAndCapturesOnRow x ys
    where makeSetofCaptures :: Int -> (Int, Int, Int) -> Bool -> [(Int, Int)]
          makeSetofCaptures row (col, right, left) first
            | first = (col, row) : makeSetofCaptures row (col, right, left) False
            | (left + right) == 0 = []
            | left == 0 = (col + right, row) : makeSetofCaptures row (col, pred right, 0) False
            | right == 0 = (col - left, row) : makeSetofCaptures row (col, 0, pred left) False
            | otherwise = (col - left, row) : (col + right, row) : makeSetofCaptures row (col, pred right, pred left) False



-- | invertBoardPieces
-- takes a board and returns the board with every piece flipped

invertBoardPieces :: Board -- ^ Takes in the current board
					-> Board -- ^ Returns a board with all the pieces flipped

invertBoardPieces [] = []
invertBoardPieces (x:xs) = (invertRowPieces x) : (invertBoardPieces xs)

-- | invertRowPieces
-- subrutine of invertBoardPieces

invertRowPieces :: [Cell] -- ^ Takes in the list of piece owners in a row
				-> [Cell] -- ^ Returns the list of inverted piece owners in a row

invertRowPieces [] = []
invertRowPieces (x:xs) = (otherCell x) : (invertRowPieces xs)

-- | mapMoves will take an (Int, Int) pair from a board rotated 45, 90 or 135 degrees
--   and return its co-ordinates in standard position

mapMoves :: (Int, Int)   -- ^ The input pair to be 'back-rotated' to stnd position
         -> Int          -- ^ The number of 45 degree rotations to take the input through
         -> (Int, Int)   -- ^ Returns the pair represented in standard position

mapMoves move 0 = move
mapMoves (c,r) 1
	 |(r <= 8) = (8-(r-c),c) 
	 |otherwise = (c,(r-8)+c)
mapMoves (c,r) 2 = (9-r, c)
mapMoves (c,r) 3
	 |(r <= 8) = (9-c, 8 -(r-c))
	 |otherwise = (17-(r+c), c)



-- | compressAllMovesAndCaptures will take in a set of moves and captures from findAll and
--   return a compress version of the set. This in neccessary because captures will be missed
--   if the choosers pick a moves from an uncompressed version. For example if
--   [[(5,4)(5,5)],[(5,4),(6,4)]] is returned from the findAll function the chooser would only
--   get one of the captures from a move that should capture two.
compressAllMovesAndCaptures :: [[(Int,Int)]] -- ^ Takes in the list of moves
                            -> [[(Int,Int)]] -- ^ Returns the compressed list
compressAllMovesAndCaptures []     = []
compressAllMovesAndCaptures (d:[]) = d:[]
compressAllMovesAndCaptures (a:as) = compress a as : compressAllMovesAndCaptures (modified a as)
  where compress :: [(Int, Int)] -> [[(Int, Int)]] -> [(Int, Int)]
        compress a [] = a
        compress a (b:bs)
         | head a == head b = head b : (tail a ++ tail b)
         | otherwise        = compress a bs
        modified :: [(Int,Int)] -> [[(Int,Int)]] -> [[(Int,Int)]]
        modified a [] = []
        modified a (b:bs)
         | head a == head b = bs
         | otherwise        = b : modified a bs





----------------------------------FSM--------------------------------------

-- | State is the thing that an FSM will hold onto as it is folded across a list
--   it represents two seperate finite state machines and a memory bank to store
--   valid moves and thier captures.
--   The first two elements (Int, (Int,Int), _, _, _) represent the fsm tracking moves
--   that will capture pieces to the right of a given square, the first Int will keep
--   track of whether or not a place is valid the (Int, Int) pair will keep track of how
--   many pieces are captured and from which place in the row.
--   The next two elements are identical except they keep track of pieces captured to the
--   left of a given square.
--   The final (Int, Int, Int) tuple repreesents a type of memory and means
--   (Int -- this spot, Int -- will capture this many pieces to the right, Int --  and
--   this many pieces to the left)
type State = (Int, (Int, Int), Int, (Int,Int), [(Int, Int, Int)])


-- | fsm
--   the fsm is designed to be the function used in a call to foldr
fsm :: Cell       -- current cell be examined by fsm
       -> State   -- state before examining cell
       -> State   -- state after examining cell
fsm E (-1, _, _, _, _) = fsmInit E
fsm B (-1, _, _, _, _) = fsmInit B
fsm W (-1, _, _, _, _) = fsmInit W
fsm E s = fsmLeftCalc E (fsmRightCalc E s)
fsm B s = fsmLeftCalc B (fsmRightCalc B s)
fsm W s = fsmLeftCalc W (fsmRightCalc W s)


-- | fsmInit must be called before fsm to make sure the State is initiallized properly
fsmInit :: Cell  -- the first cell in a row
        -> State -- the state after examining the first cell
fsmInit E = (1, (1, 0), 0, (1, 0), [])
fsmInit B = (0, (1, 0), 1, (1, 0), [])
fsmInit W = (0, (1, 0), 0, (1, 0), [])


-- | this controls the manipulation of the first two elements in the State variable
--   and adds to memory when neccessary
fsmRightCalc :: Cell -> State -> State
fsmRightCalc E (rs, (rightCapturesFrom, rightCaptures), x, y, z) = (1, (succ rightCapturesFrom, 0), x, y, z)
fsmRightCalc W (rs, (rightCapturesFrom, rightCaptures), x, y, z)
    | rs == 0 = (0, (succ rightCapturesFrom, 0), x, y, z)
    | rs == 1 = (2, (rightCapturesFrom, 1), x, y, z)
    | rs == 2 = (2, (rightCapturesFrom, succ rightCaptures), x, y, z)
fsmRightCalc B (rs, (rightCapturesFrom, rightCaptures), x, y, z)
    | rs == 0 = (0, (succ rightCapturesFrom, 0), x, y, z)
    | rs == 1 = (0, (succ rightCapturesFrom, 0), x, y, z)
    | rs == 2 = (0, (succ (rightCapturesFrom + rightCaptures), 0), x, y, (rightCapturesFrom, rightCaptures, 0) : z)

-- | this controls the manipulation of the second two elements of the state variable
--   and adds to memory when neccessary
fsmLeftCalc :: Cell -> State -> State
fsmLeftCalc B (x, y, ls, (leftCapturesFrom, leftCaptures), z) = (x, y, 1, (succ leftCapturesFrom, 0), z)
fsmLeftCalc W (x, y, ls, (leftCapturesFrom, leftCaptures), z)
    | ls == 0 = (x, y, 0, (succ leftCapturesFrom, leftCaptures), z)
    | ls == 1 = (x, y, 2, (leftCapturesFrom, succ leftCaptures), z)
    | ls == 2 = (x, y, 2, (leftCapturesFrom, succ leftCaptures), z)
fsmLeftCalc E (x, y, ls, (leftCapturesFrom, leftCaptures), z)
    | ls == 0 = (x, y, 0, (succ leftCapturesFrom, leftCaptures), z)
    | ls == 1 = (x, y, 0, (succ leftCapturesFrom, leftCaptures), z)
    | ls == 2 = (x, y, 0, (newMark, 0), (newMark, 0, leftCaptures): z)
    where newMark = succ (leftCapturesFrom + leftCaptures)

-- | since the fsm's memory is not in a nice format once the fsm finishes this will help
--   clean it up. The first four elements of state are remoeved and only the memory will remain
--   with a compressed version of the valid moves
fsmMemManage :: State -> [(Int, Int, Int)]
fsmMemManage (x, y, z, a, mem) = compress mem
    where compress :: [(Int, Int, Int)] -> [(Int, Int, Int)]
          compress [] = []
          compress (x:[]) = (x:[])
          compress (x:xs:xss)
            | (first x) == (first xs) = (x `merge` xs) : compress xss
            | otherwise = (x : (compress (xs:xss)))


-- | merge is a helper function for fsmMemManage
merge :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
merge (x1, x2, x3) (y1, y2, y3)
    | x2 == 0 = (x1, y2, y3)
    | otherwise = (x1, x2, y3)

-- | finds the first in a 3-tuple
first :: (Int, Int, Int) -> Int
first (f, _, _) = f

-- | this is syntactic sugar that allows you to run the fsm without typeing so much
--  just type runFSM row with any row and the compressed memory will be returned

-- | this switches the fsm from being a function plugged into a foldl function to
--  being a function plugged into a foldr function
fsml :: State -> Cell -> State
fsml x y = fsm y x

-- | this is syntactic sugar that allows you to run the fsm without typeing so much
--  just type runFSML row with any row and the compressed memory will be returned
--  THIS IS THE VERSION OF FSM TO USE
runFSML  :: [Cell]             -- the row to be tested
         -> [(Int, Int, Int)]  -- the compressed memory
runFSML row = fsmMemManage (foldl fsml (-1, (0,0), -1, (0,0), []) row)

-- | applies rotate45CW a number of times equal to the second input
--   so that the board will be rotated 45degrees times the second 
--   input clockwise
rotateX :: [[a]] -- ^ Takes in a list of lists representing the board
		-> Int -- ^ Takes in the degree rotation
		-> [[a]] -- ^ Returns the rotated board
rotateX board 0 = board
rotateX board x = rotateX (rotate45CW board) (x-1)

-- | Rotates the board 45 degrees clockwise
rotate45CW    :: [[a]] -> [[a]]
rotate45CW [[a8],
            [a7, b8],
            [a6, b7, c8],
            [a5, b6, c7, d8],
            [a4, b5, c6, d7, e8],
            [a3, b4, c5, d6, e7, f8],
            [a2, b3, c4, d5, e6, f7, g8],
            [a1, b2, c3, d4, e5, f6, g7, h8],
            [b1, c2, d3, e4, f5, g6, h7],
            [c1, d2, e3, f4, g5, h6],
            [d1, e2, f3, g4, h5],
            [e1, f2, g3, h4],
            [f1, g2, h3],
            [g1, h2],
            [h1]] =
           [[a1, a2, a3 ,a4, a5, a6, a7, a8],
            [b1, b2, b3, b4, b5, b6, b7, b8],
            [c1, c2, c3, c4, c5, c6, c7, c8],
            [d1, d2, d3, d4, d5, d6, d7, d8],
            [e1, e2, e3, e4, e5, e6, e7, e8],
            [f1, f2, f3, f4, f5, f6, f7, f8],
            [g1, g2, g3, g4, g5, g6, g7, g8],
            [h1, h2, h3, h4, h5, h6, h7, h8]]

rotate45CW  [[a8, b8, c8, d8, e8, f8, g8, h8], 
             [a7, b7, c7, d7, e7, f7, g7, h7],
             [a6, b6, c6, d6, e6, f6, g6, h6],
             [a5, b5, c5, d5, e5, f5, g5, h5],
             [a4, b4, c4, d4, e4, f4, g4, h4],
             [a3, b3, c3, d3, e3, f3, g3, h3],
             [a2, b2, c2, d2, e2, f2, g2, h2],
             [a1, b1, c1, d1, e1, f1, g1, h1]]=
            [[a8],
             [a7, b8],
             [a6, b7, c8],
             [a5, b6, c7, d8],
             [a4, b5, c6, d7, e8],
             [a3, b4, c5, d6, e7, f8],
             [a2, b3, c4, d5, e6, f7, g8],
             [a1, b2, c3, d4, e5, f6, g7, h8],
             [b1, c2, d3, e4, f5, g6, h7],
             [c1, d2, e3, f4, g5, h6],
             [d1, e2, f3, g4, h5],
             [e1, f2, g3, h4],
             [f1, g2, h3],
             [g1, h2],
             [h1]]       


