--
-- *****************************************
-- Implémentation Haskell de l'algorithme de compression/decompression LZW
-- *****************************************
import Data.Char
import Test.QuickCheck
import Data.Word
import Data.Word.Odd
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Control.Monad
import System.IO
-- *****************************************
-- DIVERS
-- *****************************************
fromJust :: Maybe a -> a
fromJust (Just x) = x

stringToWords8 :: String -> [Lettre]
stringToWords8 xs = map (\x -> fromIntegral (ord x)) xs

words8ToString :: [Lettre] -> String
words8ToString xs = map (\x -> chr (fromIntegral x)) xs



-- *****************************************
-- DECLARATION DES TYPES ET CLASSES
-- *****************************************
type Int10 = Word10
type Code = Word16
type Lettre = Word8
type Mot = [Lettre]

class Table a where
	empty :: a
	insert :: a -> Mot -> a
	codeOf :: a -> Mot -> Maybe Code
	stringOf :: a -> Code -> Maybe Mot
	isIn :: a -> Mot -> Bool
	split :: a -> Mot -> (Mot, Maybe Code, Mot)

data AssoList = C (Mot,Code) AssoList | Vide

data APref = N [(Lettre, Code, APref)]




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
-- INSTANCIATION D'APREF A SHOW
-- *****************************************
instance Show APref where
	show (N []) = []
	show (N ((ch,co,sa):suite)) = "("++(show ch)++","++(show co)++",["++(show sa)++"])"++(show (N suite))




-- *****************************************
-- INSTANCIATION D'ASSOLIST A TABLE
-- *****************************************
prefixeAcc :: Eq a =>  [a] -> [a] -> [a] -> [a]
prefixeAcc acc [] ys = acc
prefixeAcc acc xs [] = acc
prefixeAcc acc (x:xs) (y:ys) = if x == y
				then prefixeAcc (acc++[x]) xs ys
				else acc

plusLongPrefixeAcc :: Mot -> Mot -> AssoList -> Mot
plusLongPrefixeAcc acc mot Vide = acc
plusLongPrefixeAcc acc mot (C (s,c) reste) = if (length acc) < (length (prefixeAcc [] mot s))
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
	split Vide x = ([], Nothing, x)
	split al mot = let pref = (plusLongPrefixeAcc [] mot al)
			 in (pref, (codeOf al pref), (drop (length pref) mot))
	--
--




-- *****************************************
-- INSTANCIATION D'APREF A TABLE
-- *****************************************
nBranche :: APref -> Mot -> Int
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
mapCodeMax (N (x:xs)) = let mc = (codeMax (N [x])) in
							if mc == Nothing
								then mapCodeMax (N xs)
								else ((fromJust mc):(mapCodeMax (N xs)))
codeMax :: APref -> Maybe Code
codeMax (N []) = Nothing
codeMax (N [(ch,co,(dessous))]) = let x = codeMax dessous in
									if x == Nothing
										then Just co
										else Just(max co ( fromJust (codeMax dessous)))
codeMax (N aps) = Just(foldr max (0) (mapCodeMax (N aps)))
--
insert0 :: APref -> Mot -> Code -> APref
insert0 ap [] cn = ap
insert0 (N []) (s:str) cn = (N [(s,cn,(insert0 (N []) str (cn+1)))]) 
insert0 (N branches) (c:str) cn = let x = nBranche (N branches) (c:str) in
										if x == (-1)
											then N ((c, cn,(insert0 (N[]) str (cn+1))):branches)
											else let (ch2,co2,ap2) = ((!!) branches (x-1)) in
												N ((take (x-1) branches)++[(ch2,co2,(insert0 ap2 str cn))]++(drop x branches))
--
stringOfAcc :: Mot -> APref -> Code -> Maybe Mot
stringOfAcc acc (N []) coR = Nothing
stringOfAcc acc (N ((ch, co, ap):branches)) coRech = if co == coRech
											then Just (acc++[ch])
											else let rep = stringOfAcc (acc++[ch]) ap coRech in
														if rep == Nothing
															then stringOfAcc acc (N branches) coRech
															else rep
--
splitAcc :: Mot -> Maybe Code -> APref -> Mot -> (Mot, Maybe Code, Mot)
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
	insert ap str = let x = codeMax ap in
						if x == Nothing 
							then insert0 ap str 0
							else insert0 ap str ((fromJust x)+1)
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
	stringOf ap co = stringOfAcc [] ap co
	--
	isIn ap str = (codeOf ap str) /= Nothing
	--
	--split empty x = ([],Nothing,x)
	split ap str = splitAcc [] Nothing ap str
--




-- *****************************************
-- INITIALISATION DE TABLES
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

--Met tous les éléments d'une chaine dans une table, dans l'ordre inverse.
initTable0 ::Table a => Mot -> a
initTable0 [] = empty
initTable0 (x:xs) = insert (initTable0 xs) [x]

--Met tous les éléments de la table Ascii dans une table
initTableASCII0 :: Table a => Lettre -> a -> a
initTableASCII0 255 t = insert t [255]
initTableASCII0 n t = initTableASCII0 (n+1) (insert t [n])




-- *****************************************
-- INITIALISATION DE TABLES (ASSOLIST)
-- *****************************************
--Met tous les éléments d'une chaine dans une assoList, dans l'ordre.
initTableAL :: Mot -> AssoList
initTableAL s = initTable0 (reverse (charsInAcc s []))

--Met tous les élément de la table ASCII dans une assoList
initTableASCIIAL :: AssoList
initTableASCIIAL = initTableASCII0 0 empty




-- *****************************************
-- INITIALISATION DE TABLES (APREF)
-- *****************************************
--Met tous les éléments d'une chaine dans une assoList, dans l'ordre.
initTableAP :: Mot -> APref
initTableAP s = initTable0 (reverse (charsInAcc s []))

--Met tous les élément de la table ASCII dans un arbre de préfixes
initTableASCIIAP :: APref
initTableASCIIAP = initTableASCII0 0 (N[])




-- *****************************************
-- ALGORITHMES COMPRESSION / DECOMPRESSION
-- *****************************************
--ENCODE AVEC ACCUMULATEUR
lzwEncode0 :: Table a => a -> Mot -> [Code]
lzwEncode0 al [] = []
lzwEncode0 al texte = let (pref, mc, suff) = split al texte
			in if suff == []
				then [(fromJust mc)]
				else ((fromJust mc):(lzwEncode0 (insert al (pref++[(head suff)])) suff))

--ENCODE AVEC ACCUMULATEUR, AVEC DEBUG : AFFICHAGE DE LA TABLE AVEC
lzwEncode0D :: Table a => a -> Mot -> ([Code],a)
lzwEncode0D al texte = let (pref, mc, suff) = split al texte
			in if suff == []
				then ([(fromJust mc)],al)
				else let (cs, t) = (lzwEncode0D (insert al (pref++[(head suff)])) suff)
					in (((fromJust mc):cs),t)	

--DECODAGE AVEC ACCUMULATEUR
lzwDecode0 :: Table a => a -> Mot -> [Code] -> Mot 
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




-- *****************************************
-- APPELS COMPRESSION / DECOMPRESSION ASSOLIST
-- *****************************************
--ENCODAGE DIRECT EN ASCII (A:ASCII)
lzwEncodeA_AL :: Mot -> [Code]
lzwEncodeA_AL str = lzwEncode0 initTableASCIIAL str

--ENCODAGE DIRECT EN ASCII (A:ASCII), AVEC DEBUG
lzwEncodeAD_AL :: Mot -> ([Code],AssoList)
lzwEncodeAD_AL str = lzwEncode0D initTableASCIIAL str

--ENCODAGE AVEC TABLE MINIMALE (M:MINIMALE)
lzwEncodeM_AL :: Mot -> [Code]
lzwEncodeM_AL str = lzwEncode0 (initTableAL str) str

--ENCODAGE AVEC TABLE MINIMALE (M:MINIMALE), AVEC DEBUG
lzwEncodeMD_AL :: Mot -> ([Code],AssoList)
lzwEncodeMD_AL str = lzwEncode0D (initTableAL str) str

--DECODAGE DIRECT EN ASCII (A:ASCII)
lzwDecodeA_AL :: [Code] -> Mot
lzwDecodeA_AL codes = lzwDecode0 initTableASCIIAL [] codes

--DECODAGE AVEC TABLE PERSO (C:CUSTOM)
lzwDecodeC_AL :: Table a => a -> [Code] -> Mot
lzwDecodeC_AL table codes = lzwDecode0 table [] codes




-- *****************************************
-- APPELS COMPRESSION / DECOMPRESSION APREF
-- *****************************************
--ENCODAGE DIRECT EN ASCII (A:ASCII)
lzwEncodeA_AP :: Mot -> [Code]
lzwEncodeA_AP str = lzwEncode0 initTableASCIIAP str

--ENCODAGE DIRECT EN ASCII (A:ASCII), AVEC DEBUG
lzwEncodeAD_AP :: Mot -> ([Code],APref)
lzwEncodeAD_AP str = lzwEncode0D initTableASCIIAP str

--ENCODAGE AVEC TABLE MINIMALE (M:MINIMALE)
lzwEncodeMD_AP :: Mot -> [Code]
lzwEncodeMD_AP str = lzwEncode0 (initTableAP str) str

--ENCODAGE AVEC TABLE MINIMALE (M:MINIMALE), AVEC DEBUG
lzwEncodeM_AP :: Mot -> ([Code],APref)
lzwEncodeM_AP str = lzwEncode0D (initTableAP str) str

--DECODAGE DIRECT EN ASCII (A:ASCII)
lzwDecodeA_AP :: [Code] -> Mot
lzwDecodeA_AP codes = lzwDecode0 initTableASCIIAP [] codes

--DECODAGE AVEC TABLE PERSO (C:CUSTOM)
lzwDecodeC_AP :: Table a => a -> [Code] -> Mot
lzwDecodeC_AP table codes = lzwDecode0 table [] codes

-- ******************************************
-- PAR DEFAUT :
-- ******************************************
lzwEncode :: Mot -> [Code]
lzwEncode = lzwEncodeA_AP

lzwDecode :: [Code] -> Mot
lzwDecode = lzwDecodeA_AL

-- *****************************************
-- TESTS QUICKCHECK
-- *****************************************
propReverseA :: Mot -> Bool
propReverseA str = lzwDecode (lzwEncode str) == str 
-- quickCheckWith stdArgs { maxSuccess = 500 } propReverseA





-- *****************************************
-- ACTIONS
-- *****************************************
mainEnc :: String -> String -> IO ()
mainEnc fi fo = do
			contenu <- BL.readFile fi
			BL.writeFile fo (runPut (putCode (lzwEncode (BL.unpack contenu))))

--main :: String -> String -> IO ()
main :: IO ()
--main fi fo = do
main = do
			contenu <- BL.readFile "compresse"
			BL.writeFile "decompresse" (BL.pack (lzwDecode (runGet getCode contenu))) 
						

putCode :: [Word16] -> Put
putCode [x] = do
				putWord16le x
putCode (x:xs) = do
					putWord16le x
					putCode xs


getCode :: Get ([Word16])
getCode = do
		fin <- isEmpty
		if fin
			then return []
			else do
					c <- getWord16le
					suite <- getCode
					return (c:suite)
