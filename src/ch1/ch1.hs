module Ch1 where

-- Chapter 1 Example
-- tried to rewrite / reimplement basic sum function, but ghci was having none of it.
-- yay! worked when I renamed it to sum' and also unindented the definitions below declaration
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (n:ns) = n + sum' ns

-- Chapter 1
-- Exercises problem 1 :: implementation of double
double x = 2 * x

-- Chapter 1
-- Exercises problem 3 :: product ::
-- yay! it works!
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- Chapter 1 Example
-- quick sort example
-- note that qsort is very general, and can work with any ordinal type (e.g. can be ordered)
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

-- Chapter 1
-- Exercises problem 4 :: reverse quick sort
rsort :: Ord a => [a] -> [a]
rsort [] = []
rsort (x:xs) = rsort left ++ [x] ++ rsort right
                where
                    left = [a | a <- xs, a >= x]
                    right = [b | b <- xs, b < x]

-- Chapter 1
-- Exercises problem 5 :: replacing <= by < ::
qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
                where
                    smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b > x]
-- the result?
-- it does not deal with repeated values...
-- *Main Lib> qsort' [2, 2, 3, 1, 1]
-- [1,2,3]

-- Chapter 1
-- Exercises problem 5 :: continued...
-- however, you could then put the equals back in on the > side of things and see.
qsort'' :: Ord a => [a] -> [a]
qsort'' [] = []
qsort'' (x:xs) = qsort'' smaller ++ [x] ++ qsort'' larger
                where
                    smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b >= x]

-- note that the behavior of qsort'' and qsort is the same, as far as I can tell.

-- Chapter 1 Example
-- sequence of actions example
seqn :: Monad m => [m a] -> m [a]
-- a simpler, less general declaration:
-- seqn :: [IO a] -> IO [a]
seqn []             = return []
seqn (act: acts)    = do x <- act
                         xs <- seqn acts
                         return (x:xs)
