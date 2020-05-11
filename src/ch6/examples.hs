module Examples where

---------------
-- Section 6.1 : recursion

-- simple factorial function
-- defined directly
fac' :: Int -> Int
fac' n = product [1..n]
-- or recursively
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- although it's clear that in this case the non-recursive implementation is much simpler,
-- there are many cases where recursion presents a simple and natural approach
-- furthermore, as we'll see in Chapter 16, defining functions using recursion allows properties of those functions to be proved via induction

-- a recursive definition of (our own) multiplication for NON-NEGATIVE integers
-- any operators that are all symbols will be automatically infix in Haskell
(.*) :: Int -> Int -> Int
m .* 0 = 0
m .* n = m + (m .* (n-1))

---------------
-- Section 6.2 : recursing on Lists

-- impelementing basic library functions via recursion
product' :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product' ns

length' :: [a] -> Int
length' []      = 0
length' (_:xs)  = 1 + length' xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

(.++) :: [a] -> [a] -> [a]
[] .++ ys = ys
(x:xs) .++ ys = x : (xs .++ ys)

-- defining an insert function which will insert a value into a sorted list
insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []        = []
isort (x:xs)    = insert x (isort xs)


---------------
-- Section 6.3 : recursing on Multiple arguments

-- note the need for two base cases
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

---------------
-- Section 6.4 : 'multiple recursion'

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
                    where
                        smaller = [a | a <- xs, a <= x]
                        larger = [b | b <- xs, b > x]

---------------
-- Section 6.5 : 'mutual recursion'
-- where two functions are defined recursively in terms of each other

-- not sure why you'd ever want to do this, but...
-- also, this works for non-negative Int values only
even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)

-- selecting items from all even or odd _index_positions_ in a list

-- if you want all even positions, start with the zero element then alternate from there
evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

-- if you want all odd positions, skip the zero'th element, and start building your list from the one'th element, which you've now passed as the zero'th element to the evens function... If that makes sense ;-)
odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

---------------
-- Section 6.6 : ...like riding a bike... three examples of a five-step process...

-- example 1
-- the product of a list of numbers
    -- step 1 : define the type
-- product :: [Int] -> Int
    -- step 2 : enumerate the cases
-- product :: [Int] -> Int
-- product [] =
-- product (n:ns) =
    -- step 3 : define the simple cases / base case
    -- for product, we use the identity, 1
-- product :: [Int] -> Int
-- product [] = 1
-- product (n:ns) =
    -- step 4 : define the other cases
    -- renamed to avoid name-collision with earlier product' example
list_product :: [Int] -> Int
list_product [] = 1
list_product (n:ns) = n * list_product ns
    -- step 5 : generalize and simplify
    -- in this case, the simpler way is to define our 'list_product' in terms of foldr
    -- foldr is introduced in Chapter 7
list_product' :: Num a => [a] -> a
list_product' = foldr (*) 1


-- example 2
-- defining the drop function
-- takes an integer n and a list of values of some type a
-- produces another list of such values a, having dropped the first n values
    -- step 1 : define the type
-- drop' :: Int -> [a] -> [a]
    -- step 2 : enumerate the cases
-- drop' :: Int -> [a] -> [a]
-- drop' 0 [] =
-- drop' 0 (x:xs) =
-- drop' n [] =
-- drop' n (x:xs)
    -- step 3 : define the simplest (base) cases
-- drop' :: Int -> [a] -> [a]
-- drop' 0 [] = []
-- drop' 0 (x:xs) = x:xs
-- drop' n [] = []
-- drop' n (x:xs) =
    -- step 4 : define the other cases
-- drop' :: Int -> [a] -> [a]
-- drop' 0 [] = []
-- drop' 0 (x:xs) = x:xs
-- drop' n [] = []
-- drop' n (x:xs) = drop' (n-1) xs
    -- step 5 : generalize and simplify
    -- one could generalize to use Integral which is either Int or Integer
    -- however, for efficiency reasons, the standard Prelude _does_ restrict functions such as 'take' and 'drop' to Int
    -- combine the first two cases into one
    -- finally, look for cases where you never use a specified parameter
    -- and replace it with _ wildcard
drop' :: Integral b => b -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs


-- example 3
-- defining the init function
-- takes a list; cannot accept an empty list
-- returns the list without its last element
    -- step 1 : type signature
-- init' :: [a] -> [a]
    -- step 2 : enumerate cases
-- init' :: [a] -> [a]
-- init' (x:xs) =
    -- step 3 : define simplest cases
    -- library function null :: [a] -> Bool returns True is a list is empty
    -- removing the last element from a list with one element returns []
-- init' :: [a] -> [a]
-- init' (x:xs) | null xs = []
--              | otherwise =
    -- step 4 : define other cases
-- init' :: [a] -> [a]
-- init' (x:xs) | null xs = []
--              | otherwise = x : init' xs
    -- step 5 : generalize and simplify
    -- hashtag proud! the base case is actually the one I had literally written, earlier, but then erased when I saw the author was proceeding differently ;-)
init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs
