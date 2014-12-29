suffixes :: [a] -> [[a]]
suffixes []             = [[]]
suffixes list@(x:xs)    = list : suffixes xs
