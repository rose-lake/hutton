module Adder where

import Data.Char
import Data.Digits

-- final implementation of adder, using sequence! ;)
    -- sequence :: [IO a] -> IO [a]
    -- performs a list of actions and returns a list of resulting values

adder :: IO ()
adder = do
    x <- promptInteger "How many numbers do you wish to add together? "
    if x > 0 then do
        list_results <- sequence [addhelper n | n <- [1..x]]
        let total = sum list_results
        putStr "The total is "
        putStrLn (show total)
    else do
        putStrLn "ERROR: Please enter a positive integer"
        adder

addhelper:: Integer -> IO Integer
addhelper n = do
    x <- promptNthInteger n " number to add > "
    return x

-- this version displays a prompt and fetches back an integer
promptInteger :: String -> IO Integer
promptInteger prompt = do
    putStr prompt
    input <- getLine
    if all isDigit input then
        do  let x = read input :: Integer
            return x
    else
        do  putStrLn "ERROR: Your input did not consist only of digits"
            promptInteger prompt

-- this version displays a prompt with an 'Nth' counter at the front, and fetches back an integer
promptNthInteger :: Integer -> String -> IO Integer
promptNthInteger n prompt = do
    showNth n
    putStr prompt
    input <- getLine
    if all isDigit input then
        do  let x = read input :: Integer
            return x
    else
        do  putStrLn "ERROR: Your input did not consist only of digits"
            promptNthInteger n prompt

-- this displays 'N' in pretty-English / nice-grammar / proper-counting
showNth :: Integer -> IO ()
showNth n = do
    putStr (show n)
    let reverse_digits_n = digitsRev 10 n
    if length reverse_digits_n > 1 && reverse_digits_n !! 1 == 1 then do --this exploits Haskell lazy evaluation
        putStr "th"
    else do
        case reverse_digits_n !! 0 of
            1 -> putStr "st"
            2 -> putStr "nd"
            3 -> putStr "rd"
            _ -> putStr "th"
