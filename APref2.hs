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
	show ap = ""
--

instance Table APref where
	empty = []
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
