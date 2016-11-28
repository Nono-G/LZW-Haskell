type Code = Int

data APref = N [(Char, Code, APref)]

class Table a where
	empty :: a
	insert :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String, Maybe Code, String)


instance Show APref where
	show (N []) = []
	show (N ((ch,co,sa):suite)) = "("++(show ch)++","++(show co)++",["++(show sa)++"])"++(show (N suite))
--

instance Table APref where
	empty = N []
	--
	insert ap str = ap
	--
	codeOf ap str = Nothing
	--
	stringOf ap co = Nothing
	--
	isIn ap str = False
	--
	split ap str = ("",Nothing,"")
--

--EXEMPLES
exap1 :: APref
exap1 = N [('e',5,empty)]

exap2 :: APref
exap2 = N[('c',3,empty),('d',4,exap1)]

exap3 :: APref
exap3 = N [('a',1,exap2),('b',2,empty)]


