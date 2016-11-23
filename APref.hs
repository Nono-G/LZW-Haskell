class Table a where
	empty :: a
	ajouter :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String,Maybe Code,String)

data APref = APref [(Char,Bool,APref)] deriving (Show,Eq)
empty :: APref
empty = APref [] 

initAPref :: APref 
initAPref APref = APref [("",false,[])]



instance Table APref where
