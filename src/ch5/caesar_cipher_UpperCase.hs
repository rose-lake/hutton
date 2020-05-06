module CaesarCipherUpperCase where

------------------------
-- Chapter 5, Exercise 10:
-- Modify the Caesar Cipher example so it can also Cipher Upper Case letters...

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
letterUpperToInt :: Char -> Int
letterUpperToInt c = ord c - ord 'A'

letterLowerToInt :: Char -> Int
letterLowerToInt c = ord c - ord 'a'

-- intToLetter gives the letter represented by the given Int
-- where 'A' = 0 and 'Z' = 25
-- and 'a' = 32 and 'z' = 57
-- and there are several characters in the ASCII table that fall between 'Z' and 'a'
-- which would technically be accepted and ciphered as well,
-- except we test for isAsciiLower and isAsciiUpper in the calling method, so...
-- Int is restricted to [0..25] and [32..57]
intToLetterUpper :: Int -> Char
intToLetterUpper n = chr (ord 'A' + n)

intToLetterLower :: Int -> Char
intToLetterLower n = chr (ord 'a' + n)

-- shift will apply the Caesar Cipher to any...
    -- lower case Ascii letter :: lower -> lower
    -- Upper Case Ascii letter :: Upper -> Upper
-- any other letter or symbol will remain unchanged
-- it can accept a positive or negative Int shift
shift :: Int -> Char -> Char
shift n c   | isAsciiLower c = intToLetterLower ((letterLowerToInt c + n) `mod` 26)
            | isAsciiUpper c = intToLetterUpper ((letterUpperToInt c + n) `mod` 26)
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
-- "Kdvnhoo lv gd erpe"
-- *CaesarCipher> encode (-3) "Kdvnhoo lv gd erpe"
-- "Haskell is da bomb"

------------------------
-- PART 2 : analysing frequencies

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
            0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
            6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
-- this is an approximate frequency table for letters found in English text

full_table :: [Float]
full_table = table ++ table
-- this is the approximate frequency table for letters found in English text DOUBLED, so we can use it in a Cipher that checks upper case and lower case, both

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100
-- this calculates a percent

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- (['A'..'Z'] ++ ['a'..'z'])]
            where n = lowers xs + uppers xs
-- this calculates the frequencies of all the letters present in our String
-- the percent is out of all the other lower case letters present in the String
-- by putting the where n = lowers xs OUTSIDE of the list comprehension, we ensure that calculation only happens ONCE, instead of 26 times

-- count number of lower case letters in a String
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- count number of upper case letters in a String
uppers :: String -> Int
uppers xs = length [x | x <- xs, x >= 'A' && x <= 'Z']

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
        all_chi_squared = [chisqr (rotate n observed_freqs) full_table | n <- [0..51]]
        -- a list of all the chi_square values
        -- for observed frequencies rotated through all 52 possible alphabetic positions
        -- to account for both upper and lower case frequencies
            -- NOTE: I don't think these would be the same, linguistically speaking,
            -- but we'll just assume that they are ;-p
        observed_freqs = freqs xs

-- overview:
-- to crack a Ceasar Cipher encoded string, when we do not know the shift_factor
    -- first record the observed frequencies in the given string xs
    -- second, calculate all possible chi-squared values, rotating the observed frequencies through all 26 possible alphabetic positions
    -- finally, take the shift_factor to be the first minimum chi-squared value you encounter
-- this may not work if there is an unusual frequeny distribution in xs, or if xs is very short

-- -- example inputs for which the Cipher WORKS:
-- *CaesarCipherUpperCase> crack "kdvnhoo lv ixq"
-- "haskell is fun"
-- *CaesarCipherUpperCase> crack "Kdvnhoo Lv Ixq!!!"
-- "Haskell Is Fun!!!"
-- *CaesarCipherUpperCase> crack (encode 31 "haskell is fun")
-- "haskell is fun"
-- *CaesarCipherUpperCase> crack (encode 31 "Haskell is Da Bomb!!! Yah!!!")
-- "Haskell is Da Bomb!!! Yah!!!"
-- --this is a pangram of 33 letters, all lowers
-- *CaesarCipherUpperCase> crack (encode 14 "the quick brown fox jumps over the lazy dog")
-- "the quick brown fox jumps over the lazy dog"
-- --this is a pangram of 33 letters, including upper case letters
-- *CaesarCipherUpperCase> crack (encode 14 "the QuicK browN foX Jumps oVer tHe laZy dog, yah!")
-- "the QuicK browN foX Jumps oVer tHe laZy dog, yah!"

-- --example inputs for which the Cipher DOES NOT WORK:
-- *CaesarCipherUpperCase> crack (encode 3 "haskell")
-- "piasmtt"
-- *CaesarCipherUpperCase> crack (encode 3 "hasKeLL")
-- "piaSmTT"
-- *CaesarCipherUpperCase> crack (encode 3 "boxing wizards jump quickly")
-- "wjsdib rduvmyn ephk lpdxfgt"
-- *CaesarCipherUpperCase> crack (encode 3 "BoXiNg WiZarDs jUmP quIcklY")
-- "WjSdIb RdUvmYn ePhK lpDxfgT"
-- --this is a pangram of 31 letters
-- *CaesarCipherUpperCase> crack (encode 12 "the five boxing wizards jump quickly")
-- "dro psfo lyhsxq gsjkbnc tewz aesmuvi"
-- *CaesarCipherUpperCase> crack (encode 12 "ThE FiVe BoXiNg WizArDs jUmP qUicKly")
-- "DrO PsFo LyHsXq GsjKbNc tEwZ aEsmUvi"
