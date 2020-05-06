module Main where

import Lib
import Control.Concurrent

main :: IO ()
main = someFunc

----------------------
-- THE GAME OF LIFE --
----------------------
-- a living cell survives if it has precisely 2 or 3 living neighbors
-- an empty cell becomes a new, living cell, if it has exactly 3 living neighbors


------------------
-- IO Utilities --
------------------

-- shows the current board
showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]
            -- sequence_ :: [IO a] -> IO ()
            -- performs a list of actions in sequence, discarding their result values and returning no result

-- writes the given string at the given position
writeat :: Pos -> String -> IO ()
writeat p xs = do
    goto p
    putStr xs

-- places the cursor at the given position
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")

-- clears the terminal screen
clearscreen :: IO ()
clearscreen = putStr "\ESC[2J"

-- trying out various screen commands with varying degrees of success!
-- resource :: https://www.gnu.org/software/screen/manual/html_node/Control-Sequences.html
--
-- moves cursor UP (the 'A') x (the '4') number of lines
-- test :: IO ()
-- test = putStr "\ESC[4A"
--
-- places the cursor at the specified position. (1;1) equals the top left of the screen
-- place :: IO ()
-- place = putStr "\ESC[1;1H"


---------------------------
-- Game Board Definition --
---------------------------

-- two constants you can modify to tweak the size of our game board
width :: Int
width = 10

height :: Int
height = 10

-- by convention, the position of a character on the screen starts at TOP LEFT == (1, 1)
type Pos = (Int, Int)

-- represent the board as a list of (x,y) positions using the same convention as the screen/terminal
-- where (1,1) == left-most, top-most
-- also (4,2) == four-to-the-right-counting-from-left, two-down-counting-from-top
type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]


----------------
-- Game Logic --
----------------

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

-- for any (x,y), generate the list of all its neighbors, accounting for the wrap-around nature of the game board
neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1), (x+1,y-1),    -- top row
                          (x-1,y), (x+1,y),                 -- middle row which omits SELF (x,y)
                          (x-1,y+1), (x,y+1), (x+1,y+1)]    -- bottom row

-- takes account of the 'wrapping around' of the board at its edges, by...
    -- first, subtracting one from each x,y
    -- then, taking the remainder of dividing by the width (for x) and height (for y)
    -- then, adding one on each x,y again (since our board positions are one-based)
-- this is a basic modulus operation, adjusted for a one-based system
    -- the reason the -1 and +1 are needed is because otherwise, anytime you are at == height or == width,
    -- the modulus would bump you back down to zero and we don't have zero.
    -- so you bump it by one, and reset it after, and this keeps you in your one-based universe
-- if we were running our board on a 0,0 == left-most, top-most coordinate system,
    -- then none of this adjusting would need to happen
    -- would could happily `mod` by width and height and always end up in the correct spot!
wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

-- first, generate a list of all neighbors,
-- then, apply the filter condition (a partial function isAlive b which needs a p to complete it)
-- then, take the length of that filtered list
-- this tells you the number of live neighbors a cell has
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

-- create a list of positions, where the positions are taken from the given board,
-- and the positions meet the condition that (liveneighbs b p) returns a 2 or a 3
survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

-- this is the `naive` implementation which checks every empty cell to see if it has three living neighbors
births' :: Board -> [Pos]
births' b = [(x,y) | x <- [1..width],
                    y <- [1..height],
                    isEmpty b (x,y),
                    liveneighbs b (x,y) == 3]

-- this is the `smarter` implementation, which only checks empty cells that are neighbors of living cells
births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                     isEmpty b p,
                     liveneighbs b p == 3]
            -- neighbs :: Pos -> [Pos]
            -- b :: [Pos]
            -- map :: takes (Pos -> [Pos]) and maps it over each element of [Pos] resulting in [[Pos]]
            -- however, concat then takes the [[Pos]] and concatenates them into a single [Pos] ! :)
            -- hooray for ghci :t type analysis!

-- function to remove duplicates from a list
-- applies rmdups recursively on
-- the tail (xs) of the list, filtered on 'not equal to x', meaning the tail excluding the current head
-- so at each iteration, you are excluding the current x from being included in the new tail
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

-- to create the next generation, append the list of survivors to the list of new births
nextgen :: Board -> Board
nextgen b = survivors b ++ births b

-- this is the overall game function
life :: Board -> IO ()
life b = do
    clearscreen
    showcells b
    -- wait 500000
    threadDelay 500000      -- 500,000 microseconds == 1/2 second.
                            -- threadDelay delays this particular thread *at least* that long
                            -- no guarantees for how promptly it will be re-taken up.
    life $ nextgen b

-- dummy function to put a time delay on our display
-- used Control.Concurrent.threadDelay instead
wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]
