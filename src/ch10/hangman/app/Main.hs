module Main where

import Lib
import System.IO    -- this is so we can access hSetEcho and turn off echoing


main :: IO ()
main = hangman

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

play :: String -> IO ()
play word = do  putStr "? "
                guess <- getLine
                if guess == word then
                    putStrLn "You got it!!"
                else
                    do  putStrLn (match word guess)
                        play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

-- this function 'secretly' gets a line of text, using our own getCh to get each Char
sgetLine :: IO String
sgetLine = do   x <- getCh          -- call our own getCh, defined below.
                if x == '\n' then
                    -- base case! (or tail case, as it were!)
                    do  putChar x
                        return []
                else
                    do  putChar '-'
                        xs <- sgetLine
                        return (x:xs)

-- this function gets a character from the screen without echoing it to the screen
getCh :: IO Char
getCh = do  hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x
