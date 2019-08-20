{- PRIMEIRO DIGITO DA STRING
firstdigit :: String -> Char
firstdigit st = case (digits st) of
                []     -> '\0'
                (a:as) -> a
-}
--TAMANHO DA LISTA
my_length :: [t] -> Int
my_length [] = 0
my_length (a:as) = 1 + my_length as

--CONCATENAÇÃO
(+++) :: [t] -> [t] -> [t]
(+++) [] y = y
(+++) (x:xs) y = x : (xs +++ y)

--FORMAR TUPLAS A PARTIR DE LISTAS
my_zip :: [t] -> [u] -> [(t,u)]
my_zip (a:as) (b:bs) = (a,b) : my_zip as bs
my_zip _      _      = []

--INVERTER LISTA
my_reverse [] = []
my_reverse (a:as) = my_reverse as ++ [a]

my_id x = x

--REPETIR VARAS VEZES
my_rep 0 ch = []
my_rep n ch = ch : my_rep (n-1) ch

my_take :: Int -> [t] -> [t]
my_take _ [] = []
my_take 0 _  = []
my_take n (a:as) = a : my_take (n-1) as

my_drop :: Int -> [t] -> [t]
my_drop _ [] = []
my_drop 0 _  = []
my_drop n (a:as) = my_drop (n-1) as

--EXERCICIO
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = 
    [("Sergio","O Senhor dos Aneis"),
     ("Andre", "Duna"),
     ("Fernando", "Jonathan Strange & Mr. Norrell"),
     ("Fernando","Duna")]
--livros emprestados

--CONSULTAS
livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros ((a,b):as) p | (a == p) = b : livros as p
                    | otherwise = livros as p

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] _ = []
emprestimos ((a,b):as) l | (b == l) = a : emprestimos as l
                         | otherwise = emprestimos as l

emprestado :: BancoDados -> Livro -> Bool
emprestado = (emprestimos baseExemplo liv /= [])
{-
emprestado [] _ = False
emprestado ((a,b):as) l | (b == l) = True
                        | otherwise = emprestado as l
-}

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos baseExemplo pess = length (livros baseExemplo pess)
{-
qtdEmprestimos [] _ = 0
qtdEmprestimos ((a,b):as) p | (a == p) = 1 + qtdEmprestimos as p
                            | otherwise = 0 + qtdEmprestimos as p
-}

--ATUALIZAÇÕES
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar baseExemplo pess liv = (pess, liv) : baseExemplo
{-emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] _ = []
emprestar -}

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] _ _ = []
devolver  ((p,l):pls) pess liv | (p, l) == (pess, liv) = pls
                               | otherwise = (p,l) : devolver pls pess liv

--COMPREENSÃO DE LISTAS
doubleList xs = [2*a | a <- xs]
doubleIfEven xs = [2*a | a <- xs, isEven a]

sumPairs :: [(Int, Int)] -> [Int]
sumPairs lp = [a+b | (a,b) <- lp]

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]