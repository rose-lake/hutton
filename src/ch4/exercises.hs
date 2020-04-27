module Exercises where 
------------------------------
-- exercises from Chapter 4 --
------------------------------
halve :: [a] -> ([a], [a])
-- this function splits an even-lengthed list into two halves
halve xs =  if (even (length xs))
            then (take ((length xs) `div` 2) xs, (drop ((length xs) `div` 2) xs))
            else error "Length must be even, sorry!"

halve_b :: [a] -> ([a], [a])
halve_b xs = (take n xs, drop n xs)
                where n = length xs `div` 2

halve_b' :: [a] -> ([a], [a])
halve_b' xs = if (even (length xs))
                then (take n xs, drop n xs)
                else error "Length must be even, sorry!"
                    where n = length xs `div` 2

halve_c :: [a] -> ([a], [a])
halve_c xs = splitAt (length xs `div` 2) xs

halve_c' :: [a] -> ([a], [a])
halve_c' xs | even (length xs)  = splitAt (length xs `div` 2) xs
            | otherwise         = error "Length must be even, sorry!"


third_a :: [a] -> a
third_a xs = if length xs < 3 then error "list must have at least three elements"
             else head (tail (tail xs))

third_b :: [a] -> a
third_b xs = if length xs < 3 then error "list must have at least three elements"
             else xs !! 2

third_c :: [a] -> a
third_c []              = error "list must have at least three elements"
third_c (_:[])          = error "list must have at least three elements"
third_c (_:(_:[]))      = error "list must have at least three elements"
third_c (_:_:x:_)       = x

-- this was my initial attempt, which works, but the book's version, above, is better
-- third_c (_:(_:(x:xs)))  = x

-- safetail :: [a] -> [a] returns empty list for empty list
-- uses null :: [a] -> Bool to determine if a list is empty
safetail_a :: [a] -> [a]
safetail_a xs = if null xs then [] else tail xs

safetail_b :: [a] -> [a]
safetail_b xs | null xs     = []
              | otherwise   = tail xs

safetail_c :: [a] -> [a]
safetail_c []   = []
safetail_c xs   = tail xs

safetail_d :: [a] -> [a]
safetail_d []   = []
safetail_d (_:xs) = xs

mult_a :: Int -> Int -> Int -> Int
mult_a x y z = x * y * z

mult_b :: Int -> (Int -> (Int -> Int))
mult_b = \x -> (\y -> (\z -> x * y * z))
