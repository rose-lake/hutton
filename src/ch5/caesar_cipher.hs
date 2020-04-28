module CaesarCipher where

------------------------
-- 5.5 Caesar Cipher, extended example
-- this implementation will only encode lowercase letters

-- Notes on LIBRARY FUNCTIONS used in this code

-- ord and char
-- from Hutton:
    -- ord and char convert between Char's and their Unicode numbers
-- from Data.Char library:
    -- ord :: Char -> Int
    -- The fromEnum method restricted to the type Char.
    -- char :: Int -> Char
    -- The toEnum method restricted to the type Char.
-- from Prelude's data Char definition:
    -- To convert a Char to or from the corresponding Int value defined by Unicode, use toEnum and fromEnum from the Enum class respectively (or equivalently ord and chr).

-- isLower
-- from Data.Char library:
    -- isLower :: Char -> Bool
    -- Selects lower-case alphabetic Unicode characters (letters)

-- fromIntegral
-- from Hutton:
    -- fromIntegral :: Int -> Float
    -- converts an integer into a floating point number


import Data.Char


------------------------
-- PART 1 : the shift itself

-- letterToInt gives us the numeric offset from 'a'
-- Char is restricted to ['a'..'z']
letterToInt :: Char -> Int
letterToInt c = ord c - ord 'a'

-- intToLetter gives the letter represented by the given Int, where 'a' = 0 and 'z' = 26
-- Int is restricted to [0..26]
intToLetter :: Int -> Char
intToLetter n = chr (ord 'a' + n)

-- shift will apply the Caesar Cipher to any lower case letter it receives
-- any other letter or symbol will remain unchanged
-- it can accept a positive or negative Int shift
shift :: Int -> Char -> Char
shift n c   | isLower c = intToLetter ((letterToInt c + n) `mod` 26)
            | otherwise = c
-- example output:
-- *CaesarCipher> shift 3 'x'
-- 'a'
-- *CaesarCipher> shift (-3) 'x'
-- 'u'

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
-- for each Char x in the String xs, apply shift n x
-- return the resulting list of shifted Chars, e.g. shifted String
-- to decode a shifted string, simply enter the negative value of initial shift Int
-- example output:
-- *CaesarCipher> encode 3 "Haskell is da bomb"
-- "Hdvnhoo lv gd erpe"
-- *CaesarCipher> encode (-3) "Hdvnhoo lv gd erpe"
-- "Haskell is da bomb"

------------------------
-- PART 2 : analysing frequencies

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
            0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
            6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
-- this is an approximate frequency table for letters found in English text

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100
-- this calculates a percent

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs
-- this calculates the frequencies of all the lower-case letters present in our String
-- the percent is out of all the other lower case letters present in the String
-- by putting the where n = lowers xs OUTSIDE of the list comprehension, we ensure that calculation only happens ONCE, instead of 26 times

-- count number of lower case letters in a String
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- count how many of a given Char in a String
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

------------------------
-- PART 3 : cracking the cipher

-- use the chi-square formula
-- given:
    -- list of observed frequencies : os
    -- list of expected frequences : es
    -- length of the two lists : n
-- chi-square = summation from zero to n-1 of each (o - e)^2 / e
-- the lower the overall result, the closer we are to the desired frequencies

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]
-- this calculates the chi_square value for observed list os against expected list es

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs
-- this rotates the given list xs :: [a] to the LEFT by n :: Int positions

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
-- this looks up every x in xs and returns its zero-based position

crack :: String -> String
crack xs = encode (-shift_factor) xs
    where
        shift_factor = head (positions (minimum all_chi_squared) all_chi_squared)
        -- the 'position' (e.g. shift factor) of the minimum chi_square value
        -- if there is more than one minimum value, this picks the first
        all_chi_squared = [chisqr (rotate n observed_freqs) table | n <- [0..25]]
        -- a list of all the chi_square values
        -- for observed frequencies rotated through all 26 possible alphabetic positions
        observed_freqs = freqs xs
-- to crack a Ceasar Cipher encoded string, when we do not know the shift_factor
    -- first record the observed frequencies in the given string xs
    -- second, calculate all possible chi-squared values, rotating the observed frequencies through all 26 possible alphabetic positions
    -- finally, take the shift_factor to be the first minimum chi-squared value you encounter
-- this may not work if there is an unusual frequeny distribution in xs, or if xs is very short

-- example outputs:

-- --SUCCESS!
-- *CaesarCipher> crack "kdvnhoo lv ixq"
-- "haskell is fun"

-- --SUCCESS!
-- *CaesarCipher> crack (encode 31 "haskell is fun")
-- "haskell is fun"

-- --SUCCESS! (pangram, 33 letters)
-- *CaesarCipher> crack (encode 14 "the quick brown fox jumps over the lazy dog")
-- "the quick brown fox jumps over the lazy dog"

-- --NOPE
-- *CaesarCipher> crack (encode 3 "haskell")
-- "piasmtt"

-- --NOPE again
-- *CaesarCipher> crack (encode 3 "boxing wizards jump quickly")
-- "wjsdib rduvmyn ephk lpdxfgt"

-- --still NOPE (pangram, 31 letters)
-- *CaesarCipher> crack (encode 12 "the five boxing wizards jump quickly")
-- "dro psfo lyhsxq gsjkbnc tewz aesmuvi"
