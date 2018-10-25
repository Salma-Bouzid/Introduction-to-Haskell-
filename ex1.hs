
-- Exercice 1 
{-toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigits should convert positive Integers to a list of digits. (For 0 or negative inputs, toDigits should return the empty list.) 
toDigitsRev should do the same, but with the digits reversed.
Example: toDigits 1234 == [1,2,3,4]
Example: toDigitsRev 1234 == [4,3,2,1]
Example: toDigits 0 == []
Example: toDigits (-17) == []-}

toDigits :: Integer -> [Integer]
toDigits x
     | x <= 0 = []
     |otherwise = toDigits (x `div` 10) ++ [x `mod` 10]
     
     
toDigitsRev :: Integer -> [Integer]
toDigitsRev x 
     | x <= 0 = []
     |otherwise = [x `mod` 10 ] ++ toDigitsRev(x `div` 10)
     
-- Exercice 2
   
{- Once we have the digits in the proper order, we need to
double every other one. Define a function
doubleEveryOther :: [Integer] -> [Integer]
Remember that doubleEveryOther should double every other number
beginning from the right, that is, the second-to-last, fourth-to-last,
. . . numbers are doubled.
Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
Example: doubleEveryOther [1,2,3] == [1,4,3] -}

multiply :: Integer -> Integer
multiply x = x*2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther num_list = map multiply  num_list

-- Exercice 3

sumnum :: Integer -> Integer
sumnum n =   sum (toDigits n)

sumDigits :: [Integer] -> Integer
sumDigits list = sum (map sumnum list)

-- Exercice 4

validate :: Integer -> Bool
validate n 
       | (sumDigits  (doubleEveryOther (toDigits n))) `mod` 8 == 0 = True
       |otherwise = False