-- Primeira Prova de Paradigmas de Linguagens Computacionais 
-- 1/2017 - 01/05/2017
--
-- Nome: 
--
{- 1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
   do menor para o maior elemento..
   exemplo: isSorted [1,6,8,9,9] ------> True
            isSorted [1,6,8,7,9] ------> False
   Dica: verifique se sua resposta funciona para listas de tamanho ímpar.
-}
-- isSorted :: Ord t => [t] -> Bool

--  2) (2.5) O método de ordenação bubble-sort funciona da seguinte forma: 
--   cada elemento da lista de entrada é comparado com o seguinte, 
--   e se eles não estiverem em ordem (do menor para o maior) sua posição na lista resultante é trocada,
--   e a comparação continua com a nova ordem.Esse processo é repetido até que a lista esteja ordenada 
--   (nenhuma troca seja mais necessária).
--   exemplo, passo a passo: 
--       bSort [4,8,3,6,1,8] ----> compara 4 e 8, 8 e 3 (troca, pois 8 > 3), 8 e 6(troca novamente), 8 e 1 (troca novamente) e 8 e 8  
--                                   ----> [4,3,6,1,8,8]
--       repetindo o processo, temos  ---> [3,4,1,6,8,8] ---> [3,1,4,6,8,8]  ---> [1,3,4,6,8,8]
--Implemente a função bSort.
--Dica 1: use funções auxiliares, que façam parte do processo;
--Dica 2: verifique que sua solução funciona para listas de tamanho ímpar.

-- bSort :: Ord t => [t] -> [t]




{- 4) (2.5) Dada o tipo de dados Tree t, abaixo, que reresenta uma árvore binária 
com informações (valores) em seus nós, faça uma função isSortedTree que informa se uma árvore está ordenada, ou seja, os valores em nós ou folhas na sub-àrvore à esquerda são sempre menores ou iguais ao valor do nó, e os da sub-árvore à direita sempre maiores ou iguais.
-}
data Tree t = Node t (Tree t) (Tree t) 
             | Leaf t
-- isSortedTree testeOrdenado ----> True
-- isSortedTree testeNaoOrdenado ----> False
-- isSortedTree :: Ord t => Tree t -> Bool

--1)
isSorted :: (Ord t) => [t] -> Bool
isSorted [] = True
isSorted [a] = True
--isSorted (a:[]) = True
isSorted (a:b:abs) = (a <= b) && isSorted (b:abs)
--isSorted (a:as) = a <= (head as) && isSorted as

--2)
bSort :: (Ord t) => [t] -> [t]
bSort t
    | isSorted t = t
    | otherwise = bSort (bSort_aux t)

bSort_aux :: (Ord t) => [t] -> [t]
bSort_aux [] = []
bSort_aux [a] = [a]
bSort_aux (a:b:abs)
    | (a <= b) = a : bSort_aux (b:abs)
    | otherwise = b : bSort_aux (a:abs)

--3) SEMPRE QUEBRAR O PROBLEMA EM PROBLEMAS MENORES
isSortedTree :: (Ord t) => Tree t -> Bool
isSortedTree (Leaf n) = True
isSortedTree (Node v tr1 tr2) = ((values tr1) `lessThan` v) && ((values tr2) `greaterThan` v) && (isSortedTree tr1) && (isSortedTree tr2)
--isSortedTree (Node v tr1 tr2) = isSorted ((values tr1) ++ [v] ++ (values tr2))

values :: (Ord t) => Tree t -> [t]
values (Leaf v) = [v]
values (Node v tr1 tr2) = (values tr1) ++ [v] ++ (values tr2)

lessThan :: (Ord t) => [t] -> t -> Bool
lessThan [] v = True
lessThan (a:as) v = (a <= v) && (as `lessThan` v)

greaterThan :: (Ord t) => [t] -> t -> Bool
greaterThan [] v = True
greaterThan (a:as) v = (a >= v) && (as `greaterThan` v)