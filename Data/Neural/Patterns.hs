-- | compute a -normalized on the "next" hyperspace (3D here)- list of patterns.  
patterns2D 	:: Int 			-- ^ number of samples in X 
		-> Int 			-- ^ number of samples in Y
		-> Double
		-> (Int,[Pattern])	-- ^ (next dimension which is 3, the list of patterns)
patterns2D sx sy k = (3,[fst . normalize k $ [f sx x,f sy y,3] 
		| x <- [0 .. sx - 1], 
		y <- if even x then [0 .. sy - 1] else [sy - 1, sy - 2.. 0]] )
	where 	f s x =  fromIntegral x /fromIntegral s



