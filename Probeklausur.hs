f (x:y:[]) = [y]
f (x:[]) = []
f (x:y:xs) = if y `mod` 3 /= 0 then y : f xs else f xs

data Baum a = Blatt a | Knoten (Baum a) a (Baum a)
instance Show a => Show (Baum a) where
  show (Blatt x) = "Blatt " ++ show x
  show (Knoten leftNode x rightNode) = "Knoten (" ++ show leftNode ++ ") " ++ show x ++ " (" ++ show rightNode ++ ")"
  
inOrder :: Baum a -> [a]
inOrder (Blatt a) = [a]
inOrder (Knoten a b c) = inOrder a ++ [b] ++ inOrder c

g :: Baum Int -> Baum Int
g (Knoten l m r) = 
    let
        markierungen:: Baum Int -> Int
        markierungen (Blatt x) = 0
        markierungen (Knoten l m r) = m + markierungen l + markierungen r 

        incrementmarkierung:: Baum Int -> Baum Int
        incrementmarkierung (Blatt x) = Blatt x
        incrementmarkierung (Knoten l m r) = Knoten (incrementmarkierung l) (m+1) (incrementmarkierung r)

        increment:: Baum Int -> Baum Int
        increment (Blatt x) = Blatt x 
        increment (Knoten l m r) = Knoten (incrementmarkierung l) m (incrementmarkierung r)
    in if (markierungen l > markierungen r) then (increment (Knoten l m r))
    else (Knoten l m r)

data Hochhaus = Hochhaus Name Hoehe Nutzung Etagen Baujahr
    deriving (Eq,Show)

type Name = String
type Hoehe = Int -- angegeben in Metern.
type Etagen = Int
type Baujahr = Int
data Nutzung = Bueros | Wohnungen | Hotel | Mehrzweck
    deriving(Eq,Show)
data Block = Block [Hochhaus] [Strasse] [Buslinie]
    deriving(Eq,Show)
type Strasse = String
type Buslinie = String

maximalHoehe :: Block -> Hoehe
maximalHoehe b = 
    let
        listeHochaus:: Block -> [Hoehe]
        listeHochaus (Block [] _ _ ) = []
        listeHochaus (Block (x:xs) s b) = haus x : listeHochaus(Block xs s b)

        haus:: Hochhaus -> Hoehe
        haus (Hochhaus _ h _ _ _) =  h 

        maxheight:: [Hoehe] -> Hoehe
        maxheight hs = maximum hs
        
    in maxheight (listeHochaus b)

ersetzeHochhaus:: String -> Hochhaus -> Block -> Block 
ersetzeHochhaus name hneu b = 
    let
        sucheHochhaus:: String -> Block -> Hochhaus -> [Hochhaus]
        sucheHochhaus name (Block hs _ _) hneu = replace name hs hneu

        replace:: String -> [Hochhaus] -> Hochhaus -> [Hochhaus]
        replace _ [] _ = []
        replace name ((Hochhaus n h nu e b ):xs) hneu = if n == name then (hneu : xs) else (Hochhaus n h nu e b) : (replace name xs hneu)

        newblock:: Block -> String -> Hochhaus -> Block
        newblock (Block hs s b) name hneu = Block (sucheHochhaus name (Block hs s b) hneu) s b
    in newblock b name hneu