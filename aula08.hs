--tipos enumerados
data Estacao = Inverno | Verao | Outono | Primavera
     deriving Show
data Temp = Frio | Quente
     deriving Show

clima :: Estacao -> Temp
clima Inverno = Frio
clima _       = Quente

--Tipos Produto(X)
showPerson :: Pessoas -> String
showPerson (Pessoa nome idade) = "Nome: " ++ nome ++ " --> Idade: " ++ show idade ++ "\n"
type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade
-- :tPessoa "José" 22
-- :tPessoa "Maria" 23
--Pessoa :: Nome -> Idade -> Pessoas

--Construtores com Argumentos
data Shape = Circle Float | Rectangle Float Float | Square Float
-- :tCircle 4.9 :: Shape
-- :tRectangle 4.2 2.0 :: Shape
isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound (Square _) = False

area :: Shape -> Int
area (Circle raio) = round (pi * raio ^ 2)
area (Rectangle base altura) = round (base * altura)
area (Square lado) = round (lado ^ 2)

{-Forma geral
data Nome_do_Tipo
    = Construtor1 t11 ... t1k1
    = Construtor2 t21 ... t2k2
    ....
    = ConstrutorN tn1 ... tnkn
-}
data Expr = Lit Int
           | Add Expr Expr
           | Sub Expr Expr

--Funções definidas recursivamente
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

--EXERCÍCIOS
showExpr :: Expr -> String
toList :: List t -> [t]
fromList :: [t] -> List t
depth :: Tree t -> Int
collapse :: Tree t -> [t]
mapTree :: (t -> u) -> Tree t -> Tree u