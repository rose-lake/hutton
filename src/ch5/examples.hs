module Examples where

-- Chapter 5 : List Comprehensions

------------------------
-- 5.1 Basic Concepts
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]
-- the generator expression _ <- xs
-- acts simply as a counter to produce the appropriate number of 1's

------------------------
-- 5.2 Guards

-- Guards are logical expressions that 'filter' the results of a list
-- Just as a later generator is more deeply nested and accessed after an earlier one, allowing later generators to actually depend on earlier ones,
-- so are guards simply evaluated and applied after the earlier generator on which they depend

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]
-- To ascertain that 15 is not prime, Haskell does not actually compute all the factors of 15.
-- As soon as any factor other than 1 or n is produced (so, 3) the function returns False.
-- that seems kind of 'magical' to me, to be honest! ;-)

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- look up all values associated with a given key in a given table
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
-- INPUT: find 'b' [('a',1), ('b',2), ('c',3), ('b',4)]
-- OUTPUT: [2, 4]

------------------------
-- 5.3 the zip function

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
-- this is a sneaky little trick!
-- tail xs offsets xs by ONE, by omitting the first element of xs
-- then, if you zip the original xs with tail of xs, you are effectively pairing 'adjacent' elements of xs
-- and zip's behavior takes care of the off-by-one in the lengths of xs and (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]
-- wow. so this now can check if a list is sorted by using the pairs function
-- then checking to see if first <= second
-- it checks that this is true for EACH pair via the 'and' operator
-- because of the 'and' operator 'short-circuit' feature, this is extra lazy!

-- THIS TRICK --
-- this is a very popular way to 'number' things in Haskell...
-- simply zip the existing list to an infinite list starting at 0 (or 1)
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
-- this looks up every x in xs and returns its position
-- the x == x' trick is similar to the k == k' trick in the table-lookup function above, aka 'find'
-- this is just a quick shorthand for saying we're looking for an element x' which is equal to our parameter x
-- INPUT: positions False [True, False, True, False]
-- OUTPUT: [1,3]
