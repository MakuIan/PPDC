--Name: Mark Ian Braun
--Martikelnummer: 8175858
import Data.List (maximumBy)
import Data.List (sort)
import Data.List (groupBy)
import Data.List (permutations)
import Data.List (sortBy)
--Question 1 
--sum of 6 Digit code
sumofDigit x1 x2 x3 = x1 + x2 + x3

calculateCheckDigit :: Int -> Int -> Int -> Int -> Int -> Int -> Int
calculateCheckDigit x1 x2 x3 x4 x5 x6 = 
     --10 - (mod (((x6 + x4+ x2) *3) + (x5 + x3 + x1)) 10)
     10 - (mod ((sumofDigit x6 x4 x2 *3) + sumofDigit x5 x3 x1) 10)
--input:calculateCheckDigit [4,2,5,4,6,1]
--output: 4

--Question 2
--a

--simple solution
permutations'' :: String -> [String]
permutations'' xs = permutations xs
--input: permutations'' "cat"
--output: ["cat","act","tac","atc","tca","cta"]

--solution that takes in a bit more work
--permutations this time only creates permutations of the list xs, which deoes not include x which is why interleave is needed
--interleave takes a character and a string and inserts the character at every position in the string
permutations' :: String -> [String]
permutations' (x:xs) = concatMap (interleave x) (permutations xs)
     where 
          interleave x [] = [[x]]
          interleave x (y:ys) = (x:y:ys): map (y:) (interleave x ys) 
--input: permutations' "cat"
--output: ["cat","act","atc","cta","tca","tac"]

--b
longestAnagrams :: [String] -> String
longestAnagrams [] = "empty"
longestAnagrams xs =   let groupedAnagrams = groupBy (\x y -> sort x == sort y) (sortBy (\x y -> compare (sort x) (sort y)) xs)
                           filteredList = filter (\x -> length x > 1) groupedAnagrams
                           longest = maximumBy (\x y -> compare (length x) (length y))  (concat filteredList)
                         in if null filteredList then "no anagrams" else longest
--input: longestAnagrams ["silent","cat","act", "listen"]
--output: "listen"

--Question 3 (1)
absdiff :: Int -> Int -> Int
absdiff x y = abs (x - y) --abs takes the absolute value of a number
--input: absdiff 5 7
--output: 2

--Question 3 (2)
square :: Int -> Int
square x = x * x
--input: square 5
--output: 25

--Question 3 (3)
sumOfSQ :: Int -> Int
sumOfSQ 0 = 0
sumOfSQ 1 = 1 
sumOfSQ x = square x + sumOfSQ (x -1)
--input: sumOfSQ 5
--output: 55
--other solution
sumOfSQ' :: Int -> Int
sumOfSQ' x = sum (map (^2) [1..x])
--input: sumOfSQ' 5
--output: 55

--Question 3 (4)
sumof :: Int -> Int
sumof 0 = 0
sumof x = x + sumof (x -1)

squareOfsum :: Int -> Int
squareOfsum 0 = 0
squareOfsum x = square (sumof x)
--input: squareOfsum 5
--output: 225

--Question 3 (5)
squareDiff :: Int -> Int
squareDiff x = absdiff (sumOfSQ x) (squareOfsum x) --absdiff x y = abs (x - y) --abs takes the absolute value of a number
--input: squareDiff 5
--output: 170