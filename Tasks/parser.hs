-- Parser for Fun0 language using functional combinators (fokker95functional.pdf)

import Prelude hiding ((<*>), (*>), (<*), fail)
import Data.Char
import Data.Either

type Parser a b = [a] -> [([a], b)]

runParser :: Parser a b -> [a] -> [([a], b)]
runParser p s = p s

symbol :: Eq s => s -> Parser s s
symbol a []     = []
symbol a (x:xs) = if a == x
                  then [(xs,x)]
                  else []

token :: Eq s => [s] -> Parser s [s]
token k xs = if k == take n xs
             then [(drop n xs, k)]
             else []
  where n = length k

satisfy :: (s -> Bool) -> Parser s s
satisfy p []     = []
satisfy p (x:xs) = [(xs,x) | p x]

succeed :: r -> Parser s r
succeed v xs = [(xs,v)]

epsilon :: Parser s ()
epsilon = succeed ()

fail :: Parser s r
fail xs = []

infixr 6 <*>, <*, *>
infixl 5 <@, <?@
infixr 4 <|>, <!>

(<*>) :: Parser s a -> Parser s b -> Parser s (a,b)
(p1 <*> p2) xs = [ (xs2, (v1,v2))
                 | (xs1, v1) <- runParser p1 xs
                 , (xs2, v2) <- runParser p2 xs1
                 ]

(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs = runParser p1 xs ++ runParser p2 xs

(<!>) :: Parser s a -> Parser s a -> Parser s a
(p1 <!> p2) xs = case (p1 <|> p2) xs of
                   []     -> []
                   (x:xs) -> [x]

sp :: Parser Char a -> Parser Char a
sp p = p . dropWhile (== ' ')

wsp :: Parser Char a -> Parser Char a
wsp p = p . filter (\x -> not (isSpace x))

just :: Parser s a -> Parser s a
just p = filter (null . fst) . p

(<@) :: Parser s a -> (a -> b) -> Parser s b
(p <@ f) xs = [ (ys, f v)
              | (ys,   v) <- p xs
              ]

type DetPars symbol result = [symbol] -> result
some :: Parser s a -> DetPars s a
some p = snd . head . just p

(<*) :: Parser s a -> Parser s b -> Parser s a
p <* q = p <*> q <@ fst

(*>) :: Parser s a -> Parser s b -> Parser s b
p *> q = p <*> q <@ snd

list (x,xs) = x:xs

many :: Parser s a -> Parser s [a]
many p =     p <*> many p <@ list
         <!> epsilon      <@ const []

many1 :: Parser s a -> Parser s [a]
many1 p = p <*> many p <@ list

option :: Parser s a -> Parser s (Maybe a)
option p =     p       <@ Just
           <!> epsilon <@ const Nothing

pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack s1 p s2 = s1 *> p <* s2

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = p <*> many (s *> p) <@ list  <!>  succeed []

chainl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl p s = p <*> many (s <*> p)
             <@ uncurry (foldl (flip ap2))

chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr p s = many (p <*> s) <*> p
             <@ uncurry (flip (foldr ap1))

ap2 (op,y) x = x `op` y
ap1 (x,op) y = x `op` y

p <?@ (no,yes) = p <@ f
  where f Nothing  = no
        f (Just x) = yes x

ziffer :: Parser Char Int
ziffer = satisfy isDigit <@ charToInt
  where charToInt c = ord c - ord '0'

ziffernToInt :: [Int] -> Int
ziffernToInt zs = foldl (\v z -> 10 * v + z) 0 zs

zahl :: Parser Char Int
zahl = many1 ziffer <@ ziffernToInt

integer :: Parser Char Int
integer =     symbol '-' *> zahl <@ negate
          <!> zahl