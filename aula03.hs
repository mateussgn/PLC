sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

double :: [Int] -> [Int]
double [] = []
double (x:xs) = (x*2) : double xs

member :: [Int] -> Int -> Bool
member [] _ = False
member (x:xs) y | x == y = True
                | otherwise = member xs y
-- member (x : xs) y = (x == y) || (member xs y)

digits :: String -> String
digits [] = []
digits (ch:chs) | (ch >= '0') && (ch <= '9') = ch : digits chs
                | otherwise = digits chs
{-digits (ch : chs) | member ['0'..'9'] ch = ch : digits chs
                    | otherwise = digits chs
-}

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs ((a,b):xs) = (a+b):sumPairs xs
--sumPairs (x:xs) = (fst x + snd x) : sumPairs xs

my_last :: [Int] -> Int
my_last [a] = a -- my_last (a:[]) = a
my_last (a:as) = my_last as

--Pegar x elementos de uma lista
my_take :: Int -> [Int] -> [Int]
my_take _ [] = []
my_take

--Tirar x elementos de uma lista
--my_drop :: Int -> [Int] -> [Int]