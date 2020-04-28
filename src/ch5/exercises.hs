module Exercises where

-- Chapter 5 Exercises

-- ex 1
ex1 = [ x^2 | x <- [1..100]]

-- ex 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]
-- returns a coordinate grid of size length(x, y) = (m,n)
-- example output:
-- *Exercises> grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

-- ex 3
-- using list comprehension and the function grid above, define square :: Int -> [(Int, Int)] that returns a coordinate square of size n that excludes the diagonal
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]
-- example output:
-- *Exercises> square 2
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]

square' :: Int -> [(Int, Int)]
square' n = [(x,y) | x <- [0..n], y <- [0..n], not (x == y)]
-- oops, this version is from scratch and doesn't use the 'grid' function

-- ex 4
-- using a similar strategy to 'length' example from the chapter
-- show how the 'replicate' library function can be implemented via list comprehension
replicate' :: Int -> a -> [a]
replicate' n x = [ x | _ <- [1..n] ]

-- ex 5
-- Pythagorean Triples
-- a triple (x,y,z) is a Pythagorean Tripele if it satisfies the equation x^2 + y^2 = z^2
-- pyths returns the list of all triples whose elements are at most n
-- define pyths using THREE GENERATORS, and a GUARD
pyths' :: Int -> [(Int, Int, Int)]
pyths' n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--------------------------------------------------------------------------------------
-- extensions on exercise 5
--------------------------------------------------------------------------------------
-- this implementation is somewhat quicker, but still relatively slow considering!
pyth_rel_prime :: Integer -> [(Integer, Integer, Integer)]
pyth_rel_prime n = [(x, y, z) | x <- [1..n], y <- [x..n], z <- [y..n], rel_prime x y z, x^2 + y^2 == z^2]
-- output only Pythagorean Triples that are relatively prime
rel_prime :: Integer -> Integer -> Integer -> Bool
rel_prime x y z = hcf x y == 1 && hcf x z == 1 && hcf y z == 1
-- relatively prime
-- for three numbers to be relatively prime, you want all their highest common factors to be 1
hcf :: Integer -> Integer -> Integer
hcf x y
    | x >= y = let remainder = x `rem` y in if remainder == 0 then y else hcf y remainder
    | y > x = let remainder = y `rem` x in if remainder == 0 then x else hcf x remainder
-- highest common factor
-- implementation based on https://www.macs.hw.ac.uk/~hwloidl/Courses/F21DP/tutorial0.html

-- this relatively prime implementation, my first attempt, is EXTREMELY SLOW
pyths_relatively_prime :: Int -> [(Int, Int, Int)]
pyths_relatively_prime n = [ (x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n],
                                relatively_prime x y z, x^2 + y^2 == z^2]
relatively_prime :: Int -> Int -> Int -> Bool
relatively_prime x y z = and [ (x' /= y') && (y' /= z') && (x' /= z') |
                                x' <- factors' x, y' <- factors' y, z' <- factors' z,
                                x' /= 1, y' /= 1, z' /=1 ]
factors' :: Int -> [Int]
factors' n = [x | x <- [1..n], n `mod` x == 0]

-- These Pythagorean Triples have no redundant triples, meaning no x y flip-flops
pyths_no_repeats :: Int -> [(Int, Int, Int)]
pyths_no_repeats n = [ (x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]
--------------------------------------------------------------------------------------

-- ex 6
-- a perfect number is the sum of all its factors excluding itself
-- use list comprehension and the function factors
-- to return a list of all perfect numbers up to given limit
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum $ factors x) - x == x ]
-- example output:
-- perfects 500
-- [6, 28, 496]
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
-- perfects is very fast up to 496,
-- but quite slow when asked to include the next perfect number which comes in at 8128

-- ex 7
-- show how
    -- [(x,y) | x <- [1, 2], y <- [3,4]]
    -- one comprehension with two generators
-- can be re-expressed as
    -- two comprehensions each with a single generator
-- Hint: nest one comprehension inside the other and make use of concat :: [[a]] -> [a]
ex7a = concat [[(x,y) | y <- [3, 4]] | x <- [1, 2]]
ex7b = concat [[(x,y) | x <- [1, 2]] | y <- [3, 4]]
-- thanks to https://github.com/evturn/programming-in-haskell/blob/master/05-list-comprehensions/05.7-exercises.hs for providing the answer! ;-)

-- ex 8
-- redefine the function positions using the function find
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
-- find looks up all values associated with a given key in a given table
-- INPUT: find 'b' [('a',1), ('b',2), ('c',3), ('b',4)]
-- OUTPUT: [2, 4]

original_positions :: Eq a => a -> [a] -> [Int]
original_positions x xs = [i | (x',i) <- zip xs [0..], x == x']
-- positions looks up every x in xs and returns its position
-- the x == x' trick is similar to the k == k' trick in the table-lookup function above, aka 'find'
-- this is just a quick shorthand for saying we're looking for an element x' which is equal to our parameter x
-- INPUT: positions False [True, False, True, False]
-- OUTPUT: [1,3]


-- ex 9
