module Examples where

-----------------------------
-- examples from Chapter 4 --
-----------------------------

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

recip' :: Fractional a => a -> a
recip' n = 1/n

-- you can create functions using the conditional if _ then _ else _
abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

signum' :: Int -> Int
signum' n = if n < 0 then -1 else
                if n == 0 then 0 else 1

-- you can use equations with guards instead of functions built with conditionals
abs_guard n | n >= 0    = n
            | otherwise = -n

signum_guard n   | n < 0     = -1
                | n == 0    = 0
                | otherwise = 1

-- you can match patterns on tuples or lists
fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

-- test if first of three characters is an 'a'
test_three :: [Char] -> Bool
test_three ['a', _, _] = True
test_three _           = False

-- test if first of any length of characters is an 'a'
test_any :: [Char] -> Bool
test_any ( 'a' : _ ) = True
test_any _           = False

-- using the 'cons' operator for pattern matching
head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs
