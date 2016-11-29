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

data APref = N [(Char, Code, APref)]

-- *****************************************
-- INSTANCIATION D'APREF A SHOW
-- *****************************************
instance Show APref where
	show (N []) = []
	show (N ((ch,co,sa):suite)) = "("++(show ch)++","++(show co)++",["++(show sa)++"])"++(show (N suite))

-- *****************************************
-- INSTANCIATION D'APREF A TABLE
-- *****************************************
nBranche :: APref -> String -> Int
nBranche (N []) str = -1
nBranche (N ((ch,co,ap):suite)) str = if (head str) == ch
										then 1
										else let x = (nBranche (N suite) str) in
											if x == (-1)
												then (-1)
												else (1+x)
--
mapCodeMax :: APref -> [Code]
mapCodeMax (N []) = []
mapCodeMax (N (x:xs)) = (codeMax (N [x])):(mapCodeMax (N xs))
codeMax :: APref -> Code
codeMax (N []) = -1
codeMax (N [(ch,co,(dessous))]) = max co (codeMax dessous)
codeMax (N aps) = foldr max (-1) (mapCodeMax (N aps))
--
insert0 :: APref -> String -> Code -> APref
insert0 ap [] cn = ap
insert0 (N []) (s:str) cn = (N [(s,cn,(insert0 (N []) str (cn+1)))]) 
insert0 (N branches) (c:str) cn = let x = nBranche (N branches) (c:str) in
										if x == (-1)
											then N ((c, cn,(insert0 (N[]) str (cn+1))):branches)
											else let (ch2,co2,ap2) = ((!!) branches (x-1)) in
												N ((take (x-1) branches)++[(ch2,co2,(insert0 ap2 str cn))]++(drop x branches))
--
stringOfAcc :: String -> APref -> Code -> Maybe String
stringOfAcc acc (N []) coR = Nothing
stringOfAcc acc (N ((ch, co, ap):branches)) coRech = if co == coRech
											then Just (acc++[ch])
											else let rep = stringOfAcc (acc++[ch]) ap coRech in
														if rep == Nothing
															then stringOfAcc acc (N branches) coRech
															else rep
--
splitAcc :: String -> Maybe Code -> APref -> String -> (String, Maybe Code, String)
splitAcc pref co (N branches) [] = (pref, co, [])
splitAcc pref co (N branches) (c:str) = let x = nBranche (N branches) (c:str) in
							if x == (-1)
								then (pref, co, (c:str))
								else let (ch,co,ap) = ((!!) branches (x-1)) in
										splitAcc (pref++[ch]) (Just co) ap str
--
instance Table APref where
	empty = N []
	--
	insert ap str = insert0 ap str ((codeMax ap)+1)
	--
	codeOf ap [] = Nothing
	codeOf (N [(ch,co,ap)]) [x] = if ch == x
									then Just co
									else Nothing

	codeOf (N ((ch,co,(ap)):branches)) [x] = if ch == x
									then Just co
									else codeOf (N branches) [x]

	codeOf (N branches) (c:str) = let x = nBranche (N branches) (c:str) in
						if x == (-1)
							then Nothing
							else let (ch2,co2,ap2) = ((!!) branches (x-1)) in
									codeOf ap2 str
	--
	stringOf ap co = stringOfAcc "" ap co
	--
	isIn ap str = (codeOf ap str) /= Nothing
	--
	--split empty x = ("",Nothing,x)
	split ap str = splitAcc "" Nothing ap str
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
initTable0 :: String -> APref
initTable0 [] = empty
initTable0 (x:xs) = insert (initTable0 xs) [x]

--Met tous les éléments d'une chaine dans une assoList, dans l'ordre.
initTableAP :: String -> APref
initTableAP s = initTable0 (reverse (charsInAcc s ""))

--Met tous les éléments de la table Ascii dans une assoList
initTableAPASCII0 :: Int -> APref -> APref
initTableAPASCII0 256 t = t
initTableAPASCII0 n t = initTableAPASCII0 (n+1) (insert t [chr n])
initTableAPASCII :: APref
initTableAPASCII = initTableAPASCII0  0 empty

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
lzwEncodeA str = lzwEncode0 initTableAPASCII str

--ENCODAGE DIRECT EN ASCII (A:ASCII), AVEC DEBUG
lzwEncodeAD :: String -> ([Code],APref)
lzwEncodeAD str = lzwEncode0D initTableAPASCII str

--ENCODAGE AVEC TABLE PERSO (C:CUSTOM)
lzwEncodeC :: Table a => a -> String -> [Code]
lzwEncodeC table str = lzwEncode0 table str

--DECODAGE DIRECT EN ASCII (A:ASCII)
lzwDecodeA :: [Code] -> String
lzwDecodeA codes = lzwDecode0 initTableAPASCII [] codes

--DECODAGE AVEC TABLE PERSO (C:CUSTOM)
lzwDecodeC :: Table a => a -> [Code] -> String
lzwDecodeC table codes = lzwDecode0 table [] codes

-- *****************************************
-- Tests
-- *****************************************
propReverseA :: [Char] -> Bool
propReverseA str = lzwDecodeA (lzwEncodeA str) == str 
-- quickCheckWith stdArgs { maxSuccess = 500 } propReverseA
