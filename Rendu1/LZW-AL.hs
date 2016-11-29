--
-- *****************************************
-- Implémentation Haskell de l'algorithme de compression/decompression LZW
-- *****************************************
import Data.Char
import Test.QuickCheck

-- *****************************************
-- DIVERS
-- *****************************************
fromJust :: Maybe a -> a
fromJust (Just x) = x

-- *****************************************
-- DECLARATION DES TYPES ET CLASSES
-- *****************************************
type Code = Int

class Table a where
	empty :: a
	insert :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String, Maybe Code, String)

data AssoList = C (String,Code) AssoList | Vide

-- *****************************************
-- INSTANCIATION D'ASSOLIST A SHOW
-- *****************************************
bodyShow :: AssoList -> String
bodyShow Vide = ""
bodyShow (C (x,s) Vide) = "("++(show x)++","++(show s)++")"
bodyShow (C (x,s) reste) = "("++(show x)++","++(show s)++"):"++(bodyShow reste)
instance Show AssoList where
	show al = "["++(bodyShow al)++"]";

-- *****************************************
-- INSTANCIATION D'ASSOLIST A TABLE
-- *****************************************
prefixeAcc :: String -> String -> String -> String
prefixeAcc acc [] ys = acc
prefixeAcc acc xs [] = acc
prefixeAcc acc (x:xs) (y:ys) = if x == y
				then prefixeAcc (acc++[x]) xs ys
				else acc

plusLongPrefixeAcc :: String -> String -> AssoList -> String
plusLongPrefixeAcc acc mot Vide = acc
plusLongPrefixeAcc acc mot (C (s,c) reste) = if (length acc) < (length (prefixeAcc "" mot s))
						then plusLongPrefixeAcc s mot reste
						else plusLongPrefixeAcc acc mot reste

instance Table AssoList where
	empty = Vide
	--
	insert Vide x = C (x,0) Vide
	insert (C (s,c) Vide) x = (C (s,c) (C (x,c+1) Vide))
	insert (C sc suite) x = C sc (insert suite x)
	--
	codeOf Vide x = Nothing
	codeOf (C (s,c) reste) x = if s == x
					then Just c
					else codeOf reste x
	--
	stringOf Vide c = Nothing
	stringOf (C (s,c) reste) c2 = if c == c2
				then Just s
				else stringOf reste c2
	--
	isIn Vide x = False
	isIn (C (s,c) reste) x = if s == x
					then True
					else (isIn reste x) 
	--
	split Vide x = ("", Nothing, x)
	split al mot = let pref = (plusLongPrefixeAcc "" mot al)
			 in (pref, (codeOf al pref), (drop (length pref) mot))
	--

-- *****************************************
-- INITIALISATION DE TABLES (ASSOLIST)
-- *****************************************
--Contient
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) y = (x == y) || contains xs y

--Renvoie une unique occurence de chaque élément d'une liste, dans l'odre de première apparition
charsInAcc :: Eq a => [a] -> [a] -> [a]
charsInAcc [] ys = ys
charsInAcc (x:xs) ys = if ( contains ys x)
			then charsInAcc xs ys
			else charsInAcc xs (ys ++ [x])

--Met tous les éléments d'une chaine dans une assoList, dans l'ordre inverse.
initTable0 :: String -> AssoList
initTable0 [] = Vide
initTable0 (x:xs) = insert (initTable0 xs) [x]

--Met tous les éléments d'une chaine dans une assoList, dans l'ordre.
initTableAL :: String -> AssoList
initTableAL s = initTable0 (reverse (charsInAcc s ""))

--Met tous les éléments de la table Ascii dans une assoList
initTableALASCII0 :: Int -> AssoList -> AssoList
initTableALASCII0 256 t = t
initTableALASCII0 n t = initTableALASCII0 (n+1) (insert t [chr n])
initTableALASCII :: AssoList
initTableALASCII = initTableALASCII0  0 Vide

-- *****************************************
-- ALGORITHMES COMPRESSION / DECOMPRESSION
-- *****************************************
--ENCODE AVEC ACCUMULATEUR
lzwEncode0 :: Table a => a -> String -> [Code]
lzwEncode0 al [] = []
lzwEncode0 al texte = let (pref, mc, suff) = split al texte
			in if suff == []
				then [(fromJust mc)]
				else ((fromJust mc):(lzwEncode0 (insert al (pref++[(head suff)])) suff))

--ENCODE AVEC ACCUMULATEUR, AVEC DEBUG : AFFICHAGE DE LA TABLE AVEC
lzwEncode0D :: Table a => a -> String -> ([Code],a)
lzwEncode0D al texte = let (pref, mc, suff) = split al texte
			in if suff == []
				then ([(fromJust mc)],al)
				else let (cs, t) = (lzwEncode0D (insert al (pref++[(head suff)])) suff)
					in (((fromJust mc):cs),t)	

--DECODAGE AVEC ACCUMULATEUR
lzwDecode0 :: Table a => a -> String -> [Code] -> String 
lzwDecode0 table str1 [] = []
lzwDecode0 table [] (c:cs) = let output = fromJust (stringOf table c)  in
					output ++ (lzwDecode0 table output cs)

lzwDecode0 table prev (c:cs) = let mOutput = (stringOf table c) in
					if mOutput == Nothing
						then lzwDecode0 (insert table (prev++[(head prev)])) (prev++[(head prev)]) (c:cs)
						else let output = fromJust mOutput in
							let new = prev++[(head output)] in
								if (isIn table new )
									then  output++(lzwDecode0 table output cs) 
									else output++(lzwDecode0 (insert table new) output cs)

--ENCODAGE DIRECT EN ASCII (A:ASCII)
lzwEncodeA :: String -> [Code]
lzwEncodeA str = lzwEncode0 initTableALASCII str

--ENCODAGE DIRECT EN ASCII (A:ASCII), AVEC DEBUG
lzwEncodeAD :: String -> ([Code],AssoList)
lzwEncodeAD str = lzwEncode0D initTableALASCII str

--ENCODAGE AVEC TABLE PERSO (C:CUSTOM)
lzwEncodeC :: Table a => a -> String -> [Code]
lzwEncodeC table str = lzwEncode0 table str

--DECODAGE DIRECT EN ASCII (A:ASCII)
lzwDecodeA :: [Code] -> String
lzwDecodeA codes = lzwDecode0 initTableALASCII [] codes

--DECODAGE AVEC TABLE PERSO (C:CUSTOM)
lzwDecodeC :: Table a => a -> [Code] -> String
lzwDecodeC table codes = lzwDecode0 table [] codes

-- *****************************************
-- Tests
-- *****************************************
propReverseA :: [Char] -> Bool
propReverseA str = lzwDecodeA (lzwEncodeA str) == str 
-- quickCheckWith stdArgs { maxSuccess = 500 } propReverseA



