module Calc(calc) where 

calc :: String -> [Float]
calc = foldl f [] . words
  where 
    f (x:y:zs) "+" = (y + x):zs
    f (x:y:zs) "-" = (y - x):zs
    f (x:y:zs) "*" = (y * x):zs
    f (x:y:zs) "/" = (y / x):zs
    f xs y         = xx : xs where
		(xx,left)= safeHead $ reads y

safeHead :: [(Float, String)] -> (Float, String)
safeHead (x:xs) = x
safeHead [] = (0, "")
