module Main where

import Lib
import Data.Char

main :: IO ()
main = nim

-- game of nim example from Hutton Ch.10

---------------------
-- the game of nim --
---------------------

-- the main game loop, which takes the current board and player number as arguments
play :: Board -> Int -> IO ()
play board player = do
    newline
    putBoard board
    if finished board then do
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else do
        newline
        putStr "Player "
        putStrLn (show player)  -- check to see if 'print' can work here
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove: "
        if valid board row num then
            play (move board row num) (next player)
        else do
            newline
            putStrLn "ERROR: Invalid move"
            play board player

-- what this does ::
-- display the board.
-- check if the game is finished.
-- if it is, the previous player was the one who made the finishing move, so they win.
-- otherwise, prompt for a move
-- if the move is valid, update the board and continue the game with the next player
-- otherwise reprompt for a move until a valid one is entered

nim :: IO ()
nim = play initial 1
-- notes: because Haskell is a pure functional language, we need to supply the game state to the play function
-- also notice the separation of the pure functional stuff (game utilities) from the IO stuff (IO utilities)
-- generally, doing this type of separation is good practice!
-- this minimises and localizes the use of side effects


--------------------
-- game utilities --
--------------------

--switch to the next player.
next :: Int -> Int
next 1 = 2
next 2 = 1

-- we represent the board as a list comprising the number of stars that remain on each row
type Board = [Int]

-- initial board is [5, 4, 3, 2, 1]
initial :: Board
initial = [5, 4, 3, 2, 1]

-- the game is finished when all rows have no stars in them
finished :: Board -> Bool
finished = all (== 0)  -- all :: (a -> Bool) -> [a] -> Bool

-- a move is specified by a row number and the number of stars to be removed
-- a move is valid if the row contains at least as many stars as you wish to remove
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

-- you apply a valid move to a board, to then give us a new board
-- we use a list comprehension to update the number of stars that remain in each row
move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board]
    where update r n = if r == row then n-num else n
    -- update returns either n-num or n for the value of a given slot in the list
    -- it's a clever way to check the row'th index by zipping the list [1..] to board


------------------
-- IO utilities --
------------------

-- displays a row's stars :: 'row' is the row number, 'num' is how many stars to show
putRow :: Int -> Int -> IO ()
putRow row num = do
    -- print row        -- this will put an automatic new-line after whatever it prints.
    putStr $ show row   -- this allows us to print the row number in-line with what follows.
    putStr ": "
    putStrLn $ concat $ replicate num "* "

-- displays a game board
-- assumes that a game board will always have five rows
putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do
    putRow 1 a
    putRow 2 b
    putRow 3 c
    putRow 4 d
    putRow 5 e

-- gets a single digit of input from the user
getDigit :: String -> IO Int
getDigit prompt = do
    putStrLn prompt
    x <- getChar
    newline
    if isDigit x then
        return (digitToInt x)
    else do
        putStrLn "ERROR: Invalid digit"
        getDigit prompt

-- puts out a newline to the user interface
newline :: IO ()
newline = putChar '\n'
