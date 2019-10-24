{-
    Enis Mustafaj
    e.mustafaj@jacobs-university.de
-}
--Problem 3.3
--a)
isPrime :: Int ->Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isPrimerec n (n-1)

isPrimerec _ 1 = True
isPrimerec n t | n<=0 = error "0 or negative numbers are not allowed" 
               | n `rem` t == 0 = False 
               | otherwise =  isPrimerec n (t-1)

               
--b)
--rotate and circle function are used to generate a list with possible combinations of numbers
rotate :: Int -> [a] -> [a]
rotate 0 x = x
rotate n x = rotate (n-1) (tail x ++ [head x])

circle :: [a] -> [[a]]
circle y = [rotate x y| x<- [0..(length y - 1)]]

isPrimeCirc :: Int -> Bool
isPrimeCirc 1 =False
isPrimeCirc 2 = True
isPrimeCirc n | n<=0 = error "0 or negative numbers are not allowed" 
              | otherwise =  isPrimeCircrec (getList n) ((length $ getList n)-1)

getList :: Int -> [[Char]]
getList n = circle $ show n
isPrimeCircrec _ (-1) = True
isPrimeCircrec x t | null x == True = error "List cannot be empty"
                   | (isPrime $ read (x !! t)) == False = False 
                   | otherwise = isPrimeCircrec x (t-1)
