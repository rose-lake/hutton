module Exercises where

-- exercise 1
-- refers to the recursive factorial function defined in the chapter
-- fac :: Int -> Int
-- fac 0 = 1
-- fac n = n * fac (n-1)
-- how does the recursive factorial function behave if you pass in a negative argument?
    -- it never ends!
-- rewrite recursive factorial function to prohibit negative values
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)
      | otherwise = error "factorial is undefined for negative integers"

-- exercise 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n-1)
          | otherwise = error "this function is defined for positive integers only"

-- exercise 3
-- define your own ^ operator for non-negative integers
-- use the same pattern of recursion as for (.*) in examples.hs
(.^) :: Int -> Int -> Int
a .^ 0 = 1
a .^ b = a * (a .^ (b-1))
-- then show how it's evaluated for 2 ^ 3 using your definition
-- 2 .^ 3
-- = {applying .^}
--     2 * (2 .^ 2)
-- = {applying .^}
--     2 * (2 * (2 .^ 1))
-- = {applying .^}
--     2 * (2 * (2 * (2 .^ 0)))
-- = {applying .^ (base case)}
--     2 * (2 * (2 * (2 * 1)))
-- = {applying *}
--     8

-- exercise 4
-- Euclid's algorithm to find the greatest common divisor of two non-negative integers:
    -- if the two numbers are equal, the number is the result
    -- otherwise, the smaller is subtracted from the larger,
    -- then the same process is repeated
euclid :: (Ord a, Num a) => a -> a -> a
euclid x y  | x == y    = x
            | x > y     = euclid (x-y) y
            | x < y     = euclid x (y-x)
