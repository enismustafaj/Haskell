{--
Enis Mustafaj
e.mustafaj@jacobs-university.de
--}


{--
Problem 2.3
--}

{-- a) --}

isLeapYear :: Int -> [Char]
isLeapYear x =if x`mod`400==0 || (x `mod` 4 ==0 && x `mod` 100 /=0 )  then "Leap Year" else "Not Leap Year"
{-- b) --}

isLeapYear' y  | y `mod` 400 == 0 = "Leap year" | y `mod` 100 == 0 = "Not Leap Year"| y `mod` 4==0 = "Leap Year"| otherwise = "Not Leap Year"


{--
Problem 2.4
a)
--}

rotate :: Int -> [a] -> [a]
rotate 0 x = x
rotate n x = rotate (n-1) (tail x ++ [head x])

{-- b) --}
circle :: [a] -> [[a]]
circle y = [rotate x y| x<- [0..(length y - 1)]]
