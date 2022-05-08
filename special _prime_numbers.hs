{-
    Enis Mustafaj
    e.mustafaj@jacobs-university.de
-}
isPrime :: Int ->Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isPrimerec n (n-1)

isPrimerec _ 1 = True
isPrimerec n t | n<=0 = error "0 or negative numbers are not allowed" 
               | n `rem` t == 0 = False 
               | otherwise =  isPrimerec n (t-1)
--function to return a list of prime numbers until n
getList :: Int -> [Int]
getList n = [x | x<- [1..n], isPrime x]

isSpecialPrime :: Int -> Bool
isSpecialPrime n | n<=0 = error "0 or negative numbers are not allowed" 
                 |isPrime n == False = False
                 | otherwise = isSpecialPrimerec n ((length $ getList n) - 1) 
--function which checks if a prime is sum of 2 neighbour prime numbers and 1
isSpecialPrimerec n t | t==0 = False
                      | (+)((getList n) !! (t))  ((getList n) !! (t -1)) == n-1 = True
                      |otherwise = isSpecialPrimerec n (t-1)



