module Adder_Int where

import Data.Char

-- Chapter 10, exercise 4
adder_int :: IO ()
adder_int = do
    putStr "How many numbers do you wish to add together? "
    input <- getLine
    let n = read input :: Int
    if n > 0 then do
        total <- addhelper_int n n 0
        putStr "The total is "
        putStrLn (show total)
    else do
        putStrLn "You must enter a positive integer"
        adder_int

addhelper_int :: Int -> Int -> Int -> IO Int
addhelper_int max_n 0 total = return total
addhelper_int max_n curr_n total = do
    let nth = max_n - curr_n + 1
    x <- getDigits nth " number to add > "
    addhelper_int max_n (curr_n-1) (total+x)

-- slightly modified IO utility from Chapter 10
-- this version uses my 'nth_str' function to properly display the counting endings for 1st, 2nd, 3rd, etc.
getDigits :: Int -> String -> IO Int
getDigits nth prompt = do
    nth_str nth
    putStr prompt
    input <- getLine
    -- check to see if input is all digits
    if all_digits input then do
        -- check to see if we entered a number too long to be an Int
        -------------------------------------
        -- this is a little 'debugging code'
        -- putStrLn input
        -- let input_as_int = read input :: Int
        -- print input_as_int
        -- putStrLn $ show input_as_int
        -------------------------------------
        if isomorphic_match input then do
            let n = read input :: Int
            return n
        else do
        putStrLn "ERROR: Your input was most likely too long to convert to Int"
        getDigits nth prompt
    else do
        putStrLn "ERROR: Your input did not consist only of digits"
        getDigits nth prompt

-- my 'pretty nth output' helper func
nth_str :: Int -> IO ()
nth_str counter = do    putStr (show counter)
                        case counter of
                            1 -> putStr "st"
                            2 -> putStr "nd"
                            3 -> putStr "rd"
                            _ -> putStr "th"

-- homemade test to see if the input string consists of all digits
-- this is a LITTLE BIT of validation, but does not ensure the input can be cast into an Int
all_digits :: String -> Bool
all_digits input = (length input) == (length $ filter isDigit input)

-- just a little bit more validation
isomorphic_match :: String -> Bool
isomorphic_match input = (input == (show input_as_int))
                            where input_as_int = read input :: Int


-- slightly modified IO utility from Chapter 10
-- this version uses my 'nth_str' function to properly display the counting endings for 1st, 2nd, 3rd, etc.
-- this only fetches a SINGLE DIGIT from the user
getDigit :: Int -> String -> IO Int
getDigit nth prompt = do    nth_str nth
                            putStr prompt
                            x <- getChar
                            newline
                            if isDigit x then -- all isDigit n then
                                return (digitToInt x)
                            else
                                do  putStrLn "ERROR: Invalid digit"
                                    getDigit nth prompt

-- IO utility func from Ch.10
newline :: IO ()
newline = putChar '\n'
