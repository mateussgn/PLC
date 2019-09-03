--Função de Composição
--somaum :: Int -> Int
somaum x = x+1
double x = x*2

twice :: (t -> t) -> (t -> t)
twice f = f . f

my_iter :: Int -> (t -> t) -> (t -> t)
my_iter 0 f = id
my_iter n f = (my_iter (n-1) f) . f

--Transformar em Expressão lambda
--f :: t -> u -> v
lambda1 v w = ((\x -> \y -> x-y) w) v