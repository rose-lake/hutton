module Examples where

-- Chapter 3
bools :: [Bool]
bools = True : bools

nums :: Int -> [[Int]]
nums 0 = [[0]]
nums n = [n] : nums (n - 1)

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f x = f x

-- also :: ha!
-- doesn't matter if the end parentheses are there or not!
apply' :: (a -> b) -> (a -> b)
apply' f = f

-- note that apply is a specialized form of id
id :: a -> a
id a = a
