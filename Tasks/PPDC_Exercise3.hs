import Prelude hiding (Word)
-- Word is also in prelude, hence it is hided
import Data.Char
-- imported functions from Data.Char: toLower
import System.Environment
-- imported functions from System.Environment: getArgs
import Data.List 
-- imported functions from Data.List: sort, nub
import Data.Map hiding (map,foldr,filter,null)
-- imported functions from Data.Map: fromListWith, toList
-- to use prelude map, foldr, null and filter, (map,foldr,filter,null) in the Data.Map is hided

type Word = String
type Sentence = [Word]
type CharCount = [(Char, Int)]
-- Ex.(Word) -> "love"
-- Ex.(Sentence) -> ["I", "love", "you"]
-- Ex.(CharCount) -> [('e', 1), ('l', 1), ('o', 1), ('v', 1)]

type WordDictionary = [(Word, CharCount)]
-- Ex.(WordDictionary) -> [("love", [('e', 1), ('l', 1), ('o', 1), ('v', 1)])]

type CharCountDictionary = [(CharCount, Sentence)]
-- Ex.(CharCountDictionary) -> [([('a', 1), ('e', 1), ('t', 1)], ["eat", "tea"])]


-- This function has an error, you should fix and report it
-- For a given word it should return a list of all possible char counts
-- Example: wordCharCounts "goethe" -> [('e',2),('g',1),('h',1),('o',1),('t',1)]
wordCharCounts :: Word -> CharCount
wordCharCounts cs = zip chars (repetition chars)
    where
        chars = sort $ nub lower --chars = "eghot"
        lower  = map toLower cs
        --"/= checks for unqeual elements not equal"
        repetition  = map (\c -> length (filter (==c) lower)) 
lower cs  = map toLower cs

-- This function has an error, you should fix and report it
-- For a given sentence it should return a list of all possible char counts
-- It should use the wordCharCounts function above
sentenceCharCounts :: Sentence -> CharCount
sentenceCharCounts = wordCharCounts . concat . map reverse

-- This function has an error, you should fix and report it
-- For a given sentece it should return a word dictionary
-- For example dictCharCounts ["you","love"]  -> [("you",[('o',1),('u',1),('y',1)]),("love",[('e',1),('l',1),('o',1),('v',1)])]
dictCharCounts :: Sentence -> WordDictionary
dictCharCounts (sx) = zip sx (map wordCharCounts sx)

-- This function has errors, you should fix and report it
-- For example dictWordsByCharCounts [("vole",[('e',1),('l',1),('o',1),('v',1)]),("you",[('o',1),('u',1),('y',1)]),("love",[('e',1),('l',1),('o',1),('v',1)])]
-- should give output of [([('e',1),('l',1),('o',1),('v',1)],["love","vole"]),([('o',1),('u',1),('y',1)],["you"])]
dictWordsByCharCounts :: WordDictionary -> CharCountDictionary
dictWordsByCharCounts = toList . fromListWith (++) . concatMap  (\(w,c) -> [(c, [w])])
--dictWordsByCharCounts = toList . fromListWith (++) . map (\(w,cc) -> (cc, [w]))

-- This function has errors, you should fix and report it
-- For example wordAnagrams "velo" [([('e',1),('l',1),('o',1),('v',1)],["love","vole"]),([('o',1),('u',1),('y',1)],["you"])]
-- should give output of ["love","vole"]
wordAnagrams :: Word -> CharCountDictionary -> Sentence
wordAnagrams w ccds = concat $ map snd (filter (\x -> fst x == (wordCharCounts  w)) (ccds))
    
-- This function has an error, you should fix and report it
-- For example  charCountsSubsets [('e',1),('t',1),('a',1)]
-- should give output of [[('a',1),('e',1),('t',1)],[('e',1),('t',1)],[('a',1),('e',1)],[('e',1)],[('a',1),('t',1)],[('t',1)],[('a',1)],[]]
charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets = map wordCharCounts . nub . powerset  . ccToWord
    where
        powerset  :: Word -> [Word]
        powerset  [] = [[]]
        powerset (c:cs) = [c:cs' | cs' <- powerset cs] ++ powerset cs
        ccToWord :: CharCount -> Word   
        ccToWord cc = concat [replicate n c | (c,n) <- cc]

-- This function has errors, you should fix and report it
-- For example subtractCounts [('e',1),('l',1),('o',1),('v',1)] [('e',2),('g',1),('h',1),('o',1),('t',1)] 
-- should give output of [('g',1),('h',1),('l',1),('t',1),('v',1)]
subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts cc1 cc2 = filter (\x -> snd x > 0) $ toList $ fromListWith (-) (cc2 ++ cc1)

-- This function has no error, you don't need to change it
-- You can use this function to test your code with small sentences
-- You can also see how other functions are used
sentenceAnagrams :: Word -> Sentence -> Sentence
sentenceAnagrams w dict =  concat $ map (\x -> anagram' x sentenceCC) sentenceSS
    where
        -- sentenceCC presents all char counts of given sentence(w)
        sentenceCC = sentenceCharCounts $ map removePunctuation $ words w
        -- sentenceSS presents all subsets of given word(w)
        sentenceSS = charCountsSubsets sentenceCC
        -- allDictWordsByCharCounts presents words-char counts pairs in the dict
        allDictWordsByCharCounts = dictWordsByCharCounts $ dictCharCounts $ dict

        anagram' :: CharCount -> CharCount -> Sentence
        anagram' subset charcount
            | null restCharCount = wordAnagrams' charcount
            | otherwise = [subsetWord ++ " " ++ subsetWord' | subsetWord <- wordAnagrams' subset, subset' <- charCountsSubsets restCharCount, subsetWord' <- anagram' subset' restCharCount]
            where
                restCharCount = subtractCounts charcount subset

        wordAnagrams' :: CharCount -> Sentence
        wordAnagrams' cc = concat $ map snd (filter (\x -> fst x == cc) allDictWordsByCharCounts)



-- this is a helper function for removing punctions from the dictionary 
-- you don't need to change it
removePunctuation :: String -> String
removePunctuation = filter (\x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))

-- main function works as expected you don't need to change it
-- what it does is reading necessary arguments and calling sentenceAnagrams function
main :: IO ()
main = do 
    -- reading sentence from user
    putStrLn "Please enter a sentence: "
    sentence <- getLine
    -- reading file
    file <- readFile "words.txt"
    -- creating dict with seperating each line
    let dict = map removePunctuation $ lines file

    -- finding all anagrams of given sentence
    -- they are sorted for test
    let anagrams = sort $ sentenceAnagrams sentence dict
    let result = if anagrams == [] then "There is no anagram in the dictionary" else foldr1 (\s1 s2 -> s1 ++ "\n" ++ s2) anagrams
    putStrLn result
