type Code = Int

class Table a where
	empty :: a
	insert :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String, Maybe Code, String)

data AssoList = C (String,Code) AssoList | Vide

alV :: AssoList
alV = Vide
al1 :: AssoList
al1 = C ("a",0) (C ("b",1) (C ("ab",2) (C ("c",3) (C ("bc",4) Vide))))
--al2 :: AssoList
--al2 C () (C () (C () ()))

alLast :: AssoList -> (String,Code)
alLast (C paire Vide) = paire
alLast (C paire suite) = alLast suite

bodyShow :: AssoList -> String
bodyShow Vide = ""
bodyShow (C (x,s) Vide) = "("++(show x)++","++(show s)++")"
bodyShow (C (x,s) reste) = "("++(show x)++","++(show s)++"):"++(bodyShow reste)
instance Show AssoList where
	show al = "["++(bodyShow al)++"]";

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

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) y = (x == y) || contains xs y

charsInAcc :: Eq a => [a] -> [a] -> [a]
charsInAcc [] ys = ys
charsInAcc (x:xs) ys = if ( contains ys x)
			then charsInAcc xs ys
			else charsInAcc xs (ys ++ [x])

initTable0 :: String -> AssoList
initTable0 [] = Vide
initTable0 (x:xs) = insert (initTable0 xs) [x]

initTable :: String -> AssoList
initTable s = initTable0 (reverse (charsInAcc s ""))

fromJust :: Maybe a -> a
fromJust (Just x) = x

lzwEncode0 :: Table a => a -> String -> [Code]
lzwEncode0 al texte = let (pref, mc, suff) = split al texte
			in if suff == []
				then [(fromJust mc)]
				else ((fromJust mc):(lzwEncode0 (insert al (pref++[(head suff)])) suff))

lzwEncode0D :: Table a => a -> String -> ([Code],a)
lzwEncode0D al texte = let (pref, mc, suff) = split al texte
			in if suff == []
				then ([(fromJust mc)],al)
				else let (cs, t) = (lzwEncode0D (insert al (pref++[(head suff)])) suff)
					in (((fromJust mc):cs),t)	
lzwEncodeA :: String -> [Code]
lzwEncodeA str = lzwEncode0 (initTable str) str

lzwEncodeAD :: String -> ([Code],AssoList)
lzwEncodeAD str = lzwEncode0D (initTable str) str

--lzwDecode :: Table a => a -> [Code] -> String
