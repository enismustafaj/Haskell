{--
Enis Mustafaj
e.mustafaj@jacobs-university.de
Problem 5.3
--}
import Data.Char

--a)
toBase :: Int -> Int -> [Int]
toBase _ 0 = []
toBase b n = toBase b (n `div` b) ++  [( n `mod` b)]  

--b)
getLength n = length n
fromBase ::  Int -> [Int] -> Int
fromBase _ [] = 0
fromBase n (x:xs) = fromBase n xs + (x * n^(getLength(x:xs) - 1))

--c)
showBase :: Int -> Int -> String
showBase _ 0 = ""
showBase b n |b == 2 =showBin n
             |b == 8 = showOct n
             |b == 16 = showHex n
             |b>10 && n `mod` b >=10 = showBase b (n `div` b) ++ (show $ chr (ord 'a' + ((n `mod` b) `mod` 10)))
             |otherwise =  showBase b (n `div` b) ++ (show $ n `mod` b)


showBin :: Int -> String
showBin 0 = ""
showBin n = showBin  (n `div` 2) ++ (show $ n `mod` 2)

showOct :: Int -> String
showOct 0 = ""
showOct n = showOct  (n `div` 8) ++ (show $ n `mod` 8)

showHex :: Int -> String
showHex 0 = ""
showHex n |n `mod` 16 >= 10 = showHex  (n `div` 16) ++ (show $ chr (ord 'a' + ((n `mod` 16) `mod` 10)))
          |otherwise = showHex (n `div` 16) ++ (show $ n `mod` 16)

--d)

getInt :: String -> Int
getInt x = read x :: Int


readBase :: Int -> String -> Int
readBase _ "" = 0
readBase n (x:xs) | x == '0' = readBase n xs + 0
                  | otherwise = readBase n xs + ((*)(getInt (x:xs) `mod` 10)  (n^(getLength(x:xs) - 1)))

readBin :: String -> Int
readBin "" = 0
readBin (x:xs) | x == '0' = readBin xs + 0
               | otherwise = readBin xs + ((*)(getInt (x:xs) `mod` 10)  (2^(getLength(x:xs) - 1)))

readOct :: String -> Int
readOct "" = 0
readOct (x:xs) | x == '0' = readOct xs + 0
               | otherwise = readOct xs + ((*)(getInt (x:xs) `mod` 10)  (8^(getLength(x:xs) - 1)))

readHex :: String -> Int
readHex "" = 0
readHex (x:xs) | x == '0' = readHex xs + 0
               | otherwise = readHex xs + ((*)(getInt (x:xs) `mod` 10)  (16^(getLength(x:xs) - 1)))
