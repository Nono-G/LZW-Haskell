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
splitAcc :: String -> Maybe Code -> APref -> String -> (String, Maybe Code, String)
--splitAcc pref co (N [])
splitAcc pref co (N branches) (c:str) = let x = nBranche (N branches) (c:str) in
							if x == (-1)
								then (pref, co, (c:str))
								else let (ch,co,ap) = ((!!) branches (x-1)) in
										splitAcc (pref++[ch]) (Just co) ap str 		
--splitAcc pref 
instance Table APref where
	empty = N []
	--
	insert ap str = insert0 ap str ((codeMax ap)+1)
	--
	codeOf (N[]) str = Nothing
	--codeOf N[() "" = Nothing
	--le cas où elle retourne un code
--	codeOf ( N ( (ch0,c0,ap0):suite ) ) str = if ch0 == (head str)
	--				then Just c0
		--			else let (ch1,c1,ap1) = head suite in codeOf (tail suite) ch0++ch1 
					--else if suite == N [(ch1,c1,ap1)] then codeOf suite ch++ch1
	--codeOf ap str = 
	--
	stringOf ap co = Nothing
	--
	isIn ap str = False
	--
	split ap str = splitAcc "" Nothing ap str
--

--EXEMPLES

exap1 :: APref
exap1 = N [('e',5,empty)]

exap2 :: APref
exap2 = N[('c',3,empty),('d',4,exap1)]

exap3 :: APref
exap3 = N [('a',1,exap2),('b',2,empty)]

--('v',0,[('b',0,[('g',0,[])])])('a',1,[('c',3,[])('d',4,[('e',5,[])])])('b',2,[])
--('a',1,[('g',0,[])('c',3,[])('d',4,[('e',5,[])])])('b',2,[])
--('a',1,[('b',0,[('g',0,[])])('c',3,[])('d',4,[('e',5,[])])])('b',2,[])
--('a',1,[('c',3,[])('d',4,[('d',0,[('c',0,[])])('e',5,[])])])('b',2,[])



