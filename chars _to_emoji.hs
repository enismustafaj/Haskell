import System.IO
import Data.Char

{--
This code doesnt work on ghci compiler, 
but i have tried on an online compiler https://tio.run/#haskell and it works properly
--}

convert :: String -> String
convert xs | null $ filter (\c -> isLetter c && isAscii c) xs = dec xs
		   | otherwise = enc xs

dec :: String -> String
dec [] = ""
dec (x:xs) | fromEnum x >=128512 && fromEnum x<=128538 = (toEnum((fromEnum x ) - 128415)::Char) : dec xs
	       | fromEnum x >=128000 && fromEnum x<=128026 = (toEnum((fromEnum x ) -127935 )::Char) : dec xs
	       | otherwise = x: dec xs

enc :: String -> String
enc [] = ""
enc (x:xs) | fromEnum x >= 97 && fromEnum x <= 122 = (toEnum((fromEnum x ) + 128415)::Char) : enc xs
	       | fromEnum x >=65 && fromEnum x <=90 = (toEnum((fromEnum x ) + 127935)::Char) : enc xs
	       | otherwise = x: enc xs
main = do
	contents <- getContents
	putStr $ convert contents
