{- | This module is used for CPSC 449 for the Othello assignment.

You MUST use this file as part of your assignment to deal with boards, 
cells, etc.  This will may be tested
by linking your assignment against a modified version of this file.

Do not modify this file.

Copyright: Copyright 2015, Rob Kremer (rkremer@ucalgary.ca), University of Calgary. 
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

--  The following pragmas are only necessary for the Read class instance of 'GameBoard'
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module OthelloTools where

import Debug.Trace 

---Cells-----------------------------------------------------------
---Cells are the state of a cell: contains White, Black or is Empty

{- | The possible contents of a cell: White, Black, and Empty.
     We do NOT include "deriving (Show)" here because we use the "instance Show ..."
     so we can customize it's display from (say) "Cell E" to "_" according to the 
     cell2Char function.
-}
data Cell = W   -- ^ White
          | B   -- ^ Black
          | E   -- ^ Empty
          deriving (Eq) --deriving (Show)

-- | Customized print form of Cell
instance Show (Cell) where
         show c = [cell2Char c]

-- | Converts a Cell to a displayable Char
cell2Char       :: Cell -> Char
cell2Char W     =  'W'
cell2Char B     =  'B'
cell2Char E     =  '_'

char2Cell       :: Char -> Cell
char2Cell 'W'   = W
char2Cell 'B'   = B
char2Cell '_'   = E

{- | Flips a cell chip: Converts a Cell to its oposite W-to-B, B-to-W, and 
     E is it's own opposite.
-}
otherCell       :: Cell-> Cell
otherCell W     = B
otherCell B     = W
otherCell E     = E

-- | IO function to print a cell in a board, which is printed as '|' and the char representation of the cell.
putCell         :: Cell -> IO()
putCell c       = do putChar '|'
                     putChar (cell2Char c)
                     
---Boards--------------------------------------------------------
---A board is just a 2d (8x8) list of Cells

-- | The representation of the board (which is 8x8).
type Board      = [[Cell]] 

-- | Customize the read fucntion for Board to coorespond with the show function.
instance Read Board where
         readsPrec _ r = let allLines = lines r
                             ls = tail $ take 9 allLines 
                             rows = map (filter (/='|')) ls
                             result = map (map char2Cell) rows
                             remainder = concat $ drop 9 allLines
                         in  [(result, remainder)]

instance Show Board where show b = board2Str b

-- | The intial state of the board
initBoard       :: GameState
initBoard       = GameState (Black, Init)
                  [ [E, E, E, E, E, E, E, E],
                    [E, E, E, E, E, E, E, E],
                    [E, E, E, E, E, E, E, E],
                    [E, E, E, W, B, E, E, E],
                    [E, E, E, B, W, E, E, E],
                    [E, E, E, E, E, E, E, E],
                    [E, E, E, E, E, E, E, E],
                    [E, E, E, E, E, E, E, E] ]

-- | Print out a row in a board in the for "|?|?|?|?|?|?|?|?|". 
putRow          :: [Cell] -> IO()
putRow r        = do mapM_ putCell r
                     putStr "|\n"
                     
-- | return a row in a board in the for "|?|?|?|?|?|?|?|?|". 
row2Str        :: [Cell] -> String
row2Str []     = [] 
row2Str (x:xs) = "|" ++ show x ++ row2Str xs

{- | IO function to print out a board in the form of:

@
 _ _ _ _ _ _ _ _
|?|?|?|?|?|?|?|?|
|?|?|?|?|?|?|?|?|
|?|?|?|?|?|?|?|?|
|?|?|?|?|?|?|?|?|
|?|?|?|?|?|?|?|?|
|?|?|?|?|?|?|?|?|
|?|?|?|?|?|?|?|?|
|?|?|?|?|?|?|?|?|
@
Where the question marks a replaced with the appropriate Cell character.
-}
putBoard        :: [[Cell]] -> IO()
putBoard a      = do
                    putStr " _ _ _ _ _ _ _ _\n"
                    mapM_ putRow a
                    putStr ""
                    
-- | Return a string representation of a board in the same form as 'putBoard', above.
board2Str         :: [[Cell]] -> String
board2Str b       = " _ _ _ _ _ _ _ _\n" ++ board2Str' b
board2Str'        :: [[Cell]] -> String
board2Str' []     = [] 
board2Str' (x:xs) = row2Str x ++ "|\n" ++ board2Str' xs

-- | Return the Cell at a point from a board.
getFromBoard             :: [[a]] -> (Int,Int) -> a
getFromBoard xs pt       = xs !! snd pt !! fst pt

--
---Game state-------------------------------------------------------

-- | Represents a Player, which is slightly different from 'Cell' as a player can't be empty.
data Player     = Black | White deriving (Eq, Show, Read)

-- | Returns the opposite 'Player'.
invertPlayer       :: Player -> Player
invertPlayer Black = White
invertPlayer White = Black

-- | Given a 'Cell', return the coorespoinding 'Player'.
playerOf        :: Cell -> Player
playerOf B      = Black
playerOf W      = White

-- | Represents the type of move played in a 'GameState'.
data Played      = Played (Int, Int) -- ^ A "normal" move.
                 | Passed            -- ^ A (legal) pass (when a player CAN'T move)
                 | Goofed (Int, Int) -- ^ An illegal move, should end the game
                 | IllegalPass       -- ^ A player pass when they should have moved, should end the game
                 | Init              -- ^ No one has moved yet. 
                   deriving (Eq, Show, Read)
                   
type Play         = (Player, Played)

{- | Represents the current state of the game.
     Why am I representing this data structure as {Play,Board} when it would be a bit easier to
     represent it as {Player,Played,Board}? Because I am going to check your work by parsing your
     output, and it's a lot easer to apply the 'read' function a line containing the (Player,Played)
     pair than to do actual parsing! (See the 'show' function for GameState.)
-}
data GameState = GameState { play :: Play    -- ^ The player and play type
                           , theBoard :: Board -- ^ The actual board.
                           } deriving (Eq)

-- | Customize the print form of GameState
instance Show (GameState) where
         show g = show (play g) ++ "\n" ++ show (theBoard g) 

-- | Customize the read fucntion for Board to coorespond with the show function.
instance Read GameState where
         readsPrec _ r = let ((play,rest):[]) = readsPrec 0 r :: [(Play,String)]
                             rest' = tail rest -- read past the newline after the header
                             ((board,rest''):[]) = (readsPrec 0 rest') :: [(Board,String)]
                             result = GameState play board
                         in  [(result, rest'')]
                         

