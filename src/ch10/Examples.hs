module Examples where

import Data.Char

-- example from Chapter 10
getThreeReturnTwoChars :: IO (Char, Char)
getThreeReturnTwoChars = do

    x <- getChar

    getChar
    -- note that this is the same as
    -- _ <- getChar

    y <- getChar

    -- either of these works to make things prettier:
    putStrLn ""
    -- putStr "\n"

    return (x,y)

-- Chapter 10, exercise 1
putStr' :: String -> IO ()
putStr' xs = sequence_ (fmap putChar xs)
