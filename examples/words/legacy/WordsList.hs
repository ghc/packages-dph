

-- | A word state
data State
	= Nil
		
	-- | A piece of string containing no spaces
	| Chunk	String

	-- | A piece of string containing spaces
	| Seg 	String 		-- ^ left section
		[String] 	-- ^ words in middle
		String		-- ^ right section
	deriving Show
	
	
stateOfChar :: Char -> State
stateOfChar c
 = case c of
	' '	-> Seg "" [] ""
	_	-> Chunk [c]
	
plusState :: State -> State -> State
plusState s1 s2
 = case (s1, s2) of
	(Nil, _)			-> s2
	(_,   Nil)			-> s1
	(Chunk aa, Chunk bb)		-> Chunk (aa ++ bb)
	(Chunk aa, Seg bl bs br)	-> Seg (aa ++ bl) bs br
	(Seg al as ar, Chunk bb)	-> Seg al as (ar ++ bb)
	(Seg al as ar, Seg bl bs br)	-> Seg al (as ++ flatten [ar] ++ flatten [bl] ++ bs) br
	
stateOfString :: String -> State
stateOfString ss
	= foldl1 plusState (map stateOfChar ss) 
		
flatten [[]]		= []
flatten xx		= xx

str1	=  "Dude   I   look  into  the   looking glass I'm always sure to see"
	++ " no matter how I dodge         about, me looking      back at me."

str2	= "Wibble  "

main
 = do	-- take the filename containing the words as the first arg
	print $ stateOfString str1