answer :: Int
answer = 42

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual x y z = (x == y) && (y == z)

maxi :: Int -> Int -> Int
maxi m n | m >= n = m
         | otherwise = n

{-- apenas teoria, não roda com código
addD a b = 2 * (a + b)
         = 2 * (b + a) = addD b a--}


{--Função Fatorial--}
fat :: Int -> Int
fat x | x == 0 =1
      | otherwise = x * fat (x - 1)

fact :: Integer -> Integer
fact 0 = 1
fact x = x * fact (x-1)