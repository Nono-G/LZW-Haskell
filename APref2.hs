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
	empty = N []
	--
	insert ap str = ap
	--
	codeOf (N[]) str = Nothing
	--codeOf N[() "" = Nothing
	--le cas où elle retourne un code
	codeOf ( N ( (ch0,c0,ap0):suite ) ) str = if ch0 == (head str)
					then Just c0
					else let (ch1,c1,ap1) = head suite in codeOf (tail suite) ch0++ch1 
					--else if suite == N [(ch1,c1,ap1)] then codeOf suite ch++ch1
	--codeOf ap str = 
	--
	stringOf ap co = Nothing
	--
	isIn ap str = False
	--
	split ap str = ("",Nothing,"")
--
