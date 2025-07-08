somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

replicaChar :: Char -> Int -> [Char]
replicaChar c n
    |n == 0 = []
    |n > 0 = c: replicaChar c (n-1)

inverte :: [a] -> [a]
inverte [] = []
inverte a = reverse a

inverte2 :: [a] -> [a]
inverte2 [] = []
inverte2(a:xs) = inverte xs ++ (a:[])

ordena :: [Int] -> [Int]
ordena [] = []
ordena (x:xs) = insere x (ordena xs)


insere::Int->[Int]->[Int]
insere x [] = x:[]
insere x (c:cs)
    |x <= c = x:c:cs
    |otherwise = c:(insere x cs)

contaVizinhosIguais :: [Char] -> Int
contaVizinhosIguais [] = 0
contaVizinhosIguais (a:x)
    |x == [] = 0
    |length (a:x) == 1 = 0
    |a == head x = 1 + contaVizinhosIguais x
    |otherwise = contaVizinhosIguais x


diferentedetres :: Int -> Bool
diferentedetres a
    |a == 3 = False
    |otherwise = True

contador :: (Int->Bool) -> [Int] -> Int
contador _ [] = 0
contador f (x:xs)
    |f x = 1 + contador f xs
    |otherwise = contador f xs 

