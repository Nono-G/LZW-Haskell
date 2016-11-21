type Code = Int

class Table a where
	empty :: a
	insert :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String, Maybe Code, String)

data AssoList = C (String,Code) AssoList | Vide

alLast :: AssoList -> (String,Code)
alLast (C paire Vide) = paire
alLast (C paire suite) = alLast suite

instance Show AssoList where
	show Vide = "[]"
	show (C (x,s) reste) = "("++(show x)++","++(show s)++"):"++(show reste)

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
	split 
	split _ _ = ("aaa", Nothing, "bbb")
	--

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) y = (x == y) || contains xs y

charsInAcc :: Eq a => [a] -> [a] -> [a]
charsInAcc [] ys = ys
charsInAcc (x:xs) ys = if ( contains ys x)
			then charsInAcc xs ys
			else charsInAcc xs (ys ++ [x])

--initTable :: String -> Int -> Table


charsIn :: String -> String
charsIn xs = charsInAcc xs []

--lzwEncode :: Table a => a -> String -> [Code]


--lzwDecode :: Table a => a -> [Code] -> String
