module Exercises where

-- Chapter 2
-- Exercises problem 3 :: correct the following script; it has 3 syntactic errors

-- original script ::
-- N = a 'div' length xs
--     where
--         a = 10
--         xs = [1, 2, 3, 4, 5]

-- corrected script :: lowercase n and backticks not regular ticks
n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

-- Chapter 2
-- Exercises problem 4 :: define 'last' in terms of library functions you were shown in Chapter 2.
-- one way
last' :: Num a => [a] -> a
last' [] = error "Can't have a last element of an empty list!"
last' l = sum (drop ((length l) - 1) l)
-- another way
last'' :: [a] -> a
last'' [] = error "Can't have a last element of an empty list!"
last'' l = head (reverse l)
-- a third way
last''' :: [a] -> a
last''' [] = error "Can't have a last element of an empty list!"
last''' l = l !! (length l - 1)

-- RECURSIVE implementation !! :) :) :)
rlast :: [a] -> a
rlast [] = error "No items!"
rlast (x : []) = x
rlast (x : xs) = rlast xs
-- PROOF ::
    -- rlast [18, 12, 15]
    -- rlast [12, 15]
    -- rlast [15]
    -- 15


-- Chapter 2
-- Exercises problem 5 :: define 'init' in terms of library functions you were shown in Chapter 2.
-- one way
init' :: [a] -> [a]
init' [] = error "Can't have an init of an empty list"
init' l = take (length l - 1) l
-- another way
init'' :: [a] -> [a]
init'' [] = error "Can't have an init of an empty list"
init'' l = reverse (tail (reverse l))

-- -- RECURSIVE implementation
-- rinit :: [a] -> [a]
-- rinit (x : xs) =
