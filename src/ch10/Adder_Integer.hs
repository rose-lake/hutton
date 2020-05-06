module Adder_Integer where

import Data.Char
import Data.Digits

-- a solid version of my adder ;)
adder_integer:: IO ()
adder_integer= do
    n <- promptInteger "How many numbers do you wish to add together? "
    if n > 0 then do
        total <- addhelper n n 0
        putStr "The total is "
        putStrLn (show total)
    else do
        putStrLn "ERROR: Please enter a positive integer"
        adder_integer

addhelper :: Integer -> Integer -> Integer -> IO Integer
addhelper max_n 0 total = return total
addhelper max_n curr_n total = do
    let n = (max_n - curr_n + 1)
    x <- promptNthInteger n " number to add > "
    addhelper max_n (curr_n-1) (total+x)

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
