-- Nicholas Davi da Cruz
 
module Main where

{-    
    1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado.
-}

divisoresDeN :: Int -> [Int]
divisoresDeN n = [i | i <- [1..n-1], (mod n i) == 0]

{-
    2. Usando List Comprehension escreva uma função, chamada contaCaractere, que conte a ocorrência de um caractere específico, em uma string dada.
-}

contaCaractere :: Char -> String -> Int
contaCaractere c s = sum [1 | c' <- s, c == c']

{-
    3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada.
-}

-- Sem utilizar 'abs'
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo ints = [if k<0 then 2*(-k) else 2*k | k <- ints]

{-
    4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.
-}

-- △ABC /= △BAC -> True
-- Portanto, são triângulos diferentes e a ordem importa.

pitagoras :: Int -> [(Int, Int, Int)]
pitagoras k = let l = [1..k]
    in  [(a, b, c) | a<-l, b<-l, c<-l,
            a^2 + b^2 == c^2 ||
            a^2 + c^2 == b^2 ||
            b^2 + c^2 == a^2]

{-
    5. Números perfeitos são aqueles cuja soma dos seus divisores é igual ao próprio número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado.
-}

numerosPerfeitos :: Int -> [Int]
numerosPerfeitos k = [p | i <- [1..k], let p = sum $ divisoresDeN i, p == i]

{-
    6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.
-}

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar l1 l2 = sum [uncurry (*) t | t <- zip l1 l2]

{-
    7. Usando List Comprehension escreva uma função, chamada primeirosPrimos, que devolva uma lista contendo os n primeiros números primos a partir do número 2.
-}

primeirosPrimos :: Int -> [Int]
primeirosPrimos n = [p | p <- take n $ primos [2..], p < n]
    where
        primos :: [Int] -> [Int]
        primos (primo:resto) =
            primo:primos (filter (\x -> 0 /= mod x primo) resto)

{-
    8. Usando List Comprehension escreva uma função, chamada paresOrdenados, que devolva uma lista de par ordenados contendo uma potência de 2 e uma potência de 3 até um determinado número dado. Observe que estes números podem ser bem grandes.
-}

paresOrdenados :: Integer -> [(Integer, Integer)]
paresOrdenados n = [(n^2,n^3) | n <- [1..n]]

main = do
    putStrLn ("")
    putStrLn ("Func. 1: entrada:  0 resultado: " ++ show(divisoresDeN 0))
    putStrLn ("Func. 1: entrada: 16 resultado: " ++ show(divisoresDeN 16))

    putStrLn ("")
    putStrLn ("Func. 2: entrada: \"Frank Alcantara\" resultado: " ++ show(contaCaractere 'a' "Frank Alcantara"))

    putStrLn ("")
    putStrLn ("Func. 3: entrada: [-2..2] resultado: " ++ show(dobroNaoNegativo [-2..2]))

    putStrLn ("")
    putStrLn ("Func. 4: entrada: 10 resultado: " ++ show(pitagoras 10))

    putStrLn ("")
    putStrLn ("Func. 5: entrada: 500 resultado: " ++ show(numerosPerfeitos 500))
    
    putStrLn ("")
    putStrLn ("Func. 6: entrada: [1,3,5,7] [2,4,6,8] resultado: " ++ show(produtoEscalar [1,3,5,7] [2,4,6,8]))
    
    putStrLn ("")
    putStrLn ("Func. 7: entrada: 10 resultado: " ++ show(primeirosPrimos 10))
    
    putStrLn ("")
    putStrLn ("Func. 8: entrada: 3 resultado: " ++ show(paresOrdenados 3))
