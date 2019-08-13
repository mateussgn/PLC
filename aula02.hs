fat :: Int -> Int
fat x | x == 0 = 1
      | x == 1 = 1
      | otherwise = x * fat (x-1)

allEqual :: Int -> Int -> Int -> Bool
allEqual m n p = (n == m) && (m == p)

a4equall :: Int -> Int -> Int -> Int -> Bool
a4equall a b c d | (allEqual a b c) && (allEqual a b d) = True
--                 |(a == b) && (a == c) && (a == d) && (b == c) && (b == d) && (c == d) = True
                 | otherwise = False

equalCount :: Int -> Int -> Int -> Int
equalCount a b c | (a == b) && (b == c) = 3
                 | (a == b) = 2
                 | (a == c) = 2
                 | (b == c) = 2
                 | otherwise = 0

addEspacos1 :: Int -> String
addEspacos1 x | x == 0 = ""
              | otherwise = addEspacos1 (x-1) ++ " "

addEspacos2 :: Int -> String
addEspacos2 0 = ""
addEspacos2 x = " " ++ addEspacos2 (x-1)

paraDireita1 :: Int -> String -> String
paraDireita1 x y = addEspacos1 x ++ y

paraDireita2 :: Int -> String -> String
paraDireita2 x str = addEspacos2 x ++ str

vendas :: Int -> Float
vendas 0 = 12.0
vendas 1 = 14.0
vendas 2 = 15.0
vendas 3 = 20.0
vendas 4 = 23.0
vendas 5 = 12.0
vendas n = 0

totalVendas :: Int -> Float
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas (n-1)

mediaVendas :: Int -> Float
mediaVendas n = totalVendas n / fromIntegral (n+1)

imprimeSemanas :: Int -> String
imprimeSemanas 0 = addEspacos1 2 ++ show (0) ++ addEspacos1 8 ++ show(vendas 0) ++ "\n" ++ addEspacos1 2
imprimeSemanas n = imprimeSemanas (n-1) ++ show (n) ++ addEspacos1 8 ++ show(vendas n) ++ "\n" ++ addEspacos1 2

imprimeTotal :: Int -> String
imprimeTotal n = "\nTotal" ++ addEspacos1 6 ++ show (totalVendas n) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = "Media" ++ addEspacos1 6 ++show (mediaVendas n) ++ "\n"


imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(cabecalho
                  ++ imprimeSemanas n
                  ++ imprimeTotal n
                  ++ imprimeMedia n)

cabecalho :: String
cabecalho = "Semana    Vendas \n"