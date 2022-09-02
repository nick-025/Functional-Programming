-- Nicholas Davi da Cruz

module Main where

-- Preciso de Data.Char por causa da função 'ord'.
-- Preciso de Data.Ix por causa da função 'range'.
-- Ambas as funções mencionadas acima foram solicitadas.

import Data.Char
import Data.Ix

{-
    1. Escreva uma função chamada fatorialn que usando o operador range e a função foldr devolva o fatorial de n.
-}

fatorialn, fatorialRangeIx :: Int -> Int

fatorialn n = foldr (*) 1 [1..n]

fatorialRangeIx n = foldr (*) 1 $ curry range 1 n

{-
    2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos reais listados.
-}

quadradoReal :: [Double] -> [Double]
quadradoReal = map (** 2)

{-
    3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras. 
-}

comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras = map length

{-
    4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 que devolva o maior número entre 0 e 100000 que seja divisivel por 29.
-}

-- Fiz assim porque quero poupar linhas de código repetidas.
-- A função do exercício 5 é mais genérica e não deixa as coisas
-- Tão 'hard-coded'. Achei melhor chamar a função com o argumento
-- definido aqui, acaba saindo a mesma coisa, pois eu iria implementar
-- tudo igual, só que com o valor fixo, ao invés de passá-lo como argumento.

maiorMultiploDe29 :: Int -> Int -> Int
maiorMultiploDe29 = maiorMultiploDe 29

{-
5. Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. 
-}

maiorMultiploDe :: Int -> Int -> Int -> Int
maiorMultiploDe k a b = maximum $ filter (\j -> 0 == mod j k) [a..b]

{-
    6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que:
    𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠 = 12 + 22 + 32 + 42 ... + 𝑛2.
-}

somaQuadrados :: [Int] -> Int
somaQuadrados lista = foldr (+) 0 $ map (^2) lista

{-
    7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.
-}

comprimento :: [Int] -> Int
comprimento lista = foldl (\ignore y -> y + 1) 0 lista

{-
    8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude:
    - flip;
    - ord;
    - max;
    - min;
    - curry;
    - uncurry.
    Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.
-}

{- 1. flip :: (a -> b -> c) -> b -> a -> c
    Esta função avalia os argumentos de uma função de forma trocada, isto é: a função 'flip' recebe uma função binária e inverte a posição destes argumentos. Dessa forma, 'flip' aplicará a função aos dois argumentos de forma invertida.
-}

flipModElementos :: [Int] -> [Int]
flipModElementos = map (flip mod 3)

concatenaInvertido :: [(String, String)] -> String
concatenaInvertido lista = foldr (++) " " $ concatenaParesInvertidos lista
    where
        concatenaParesInvertidos :: [(String, String)] -> [String]
        concatenaParesInvertidos [] = []
        concatenaParesInvertidos ((x1,y1):xs) = flip (++) x1 y1 : concatenaParesInvertidos xs

{- 2. ord :: Char -> Int
    Esta é uma função unária que retorna o código de um caractere "Char". A função 'ord' pertence ao módulo Data.Char, é necessário importá-lo antes de utilizar a função. Com esta função nós podemos obter o valor individual de cada caractere de uma String, ou ainda alterar o caractere pelo seu valor, com algum tipo de incremento ou decréscimo.
-}

getCharCodes :: String -> [Int]
getCharCodes = map ord

incrementChars :: String -> String
incrementChars = map (chr . incChar)
    where
        incChar :: Char -> Int
        incChar k = 1 + ord k

{- 3. max :: Ord a => a -> a -> a
    A função 'max' é uma função binária que retorna o maior de dois argumentos.
-}

maiorDaLista :: [Int] -> Int
maiorDaLista [] = error "No maximum number was found"
maiorDaLista [x] = x
maiorDaLista (x:xs) = max x $ maiorDaLista xs

selecionaMaiores :: [Int] -> [Int] -> [Int]
selecionaMaiores [] _ = []
selecionaMaiores _ [] = []
selecionaMaiores (x:xs) (y:ys) = max x y : selecionaMaiores xs ys

{- 4. min :: Ord a => a -> a -> a
    A função 'min' é idêntica à função 'max' em sua essência. 'min' é uma função de comparação entre dois valores, dentre os quais, o menor é retornado como resultado.
-}

menorSomaTotal :: [Int] -> [Int] -> Int
menorSomaTotal lista1 lista2 = min (foldr (+) 0 lista1) (foldr (+) 0 lista2)

trocaCaracteresPorE :: String -> String
trocaCaracteresPorE s = map (min 'e') s

{- 5. curry :: ((a,b) -> c) -> a -> b -> c
    A função 'curry' transforma funções que recebem uma tupla como argumento em uma função de funções que recebem cada uma, respectivamente, um argumento, somente.
-}

somaTupla :: (Int, Int) -> Int
somaTupla (x,y) = (+) x y

{- 6. uncurry :: (a -> b -> c) -> (a, b) -> c
    A função 'uncurry' faz justamente o contrário da função 'curry'. Avalia-se uma função de uma função que retorna um resultado e retorna-se uma função que recebe uma única tupla que, então, retorna o resultado.
-}

maiorDoPar :: Int -> Int -> Int
maiorDoPar = max

main :: IO()
main = do
    putStrLn ("")
    putStrLn ("Func. 1: entrada: 10 resultado: " ++ show(fatorialn 10))
    
    putStrLn ("Func. 1: entrada: 10 resultado: " ++ show(fatorialRangeIx 10))
    
    putStrLn ("")
    putStrLn ("Func. 2: entrada: [-10, 10] resultado: " ++ show(quadradoReal [-10, 10]))

    putStrLn ("")
    putStrLn ("Func. 3: entrada: [\"HASKELL\", \"É\", \"SHOW!\"] resultado: " ++ show(comprimentoPalavras ["HASKELL", "É", "SHOW!"]))

    putStrLn ("")
    putStrLn ("Func. 4: entrada: 0 100000 resultado: " ++ show(maiorMultiploDe29 0 100000))
    
    putStrLn ("")
    putStrLn ("Func. 5: entrada: 47 0 100000 resultado: " ++ show(maiorMultiploDe 47 0 100000))

    putStrLn ("")
    putStrLn ("Func. 6: entrada: [1..10] resultado: " ++ show(somaQuadrados [1..10]))
    
    putStrLn ("")
    putStrLn ("Func. 7: entrada: [1..10] resultado: " ++ show(comprimento [1..10]))

    putStrLn ("")
    putStrLn ("Func. 8: entrada: [1..10] resultado: " ++ show(flipModElementos [1..10]))

    putStrLn ("")
    putStrLn ("Func. 9: entrada: [(\"1\", \"2\"), (\"3\", \"4\")] resultado: " ++ concatenaInvertido [("1", "2"), ("3", "4")])
    
    putStrLn ("")
    putStrLn ("Func. 10: entrada: abcdefg resultado: " ++ show(getCharCodes "abcdefg"))

    putStrLn ("")
    putStrLn ("Func. 11: entrada: abcdefg resultado: " ++ show(incrementChars "rsqhmf"))

    putStrLn ("")
    putStrLn ("Func. 12: entrada: [1, 2, 3] resultado: " ++ show(maiorDaLista [1, 2, 3]))
    putStrLn ("Func. 12: entrada: [1, 3, 2] resultado: " ++ show(maiorDaLista [1, 3, 2]))
    putStrLn ("Func. 12: entrada: [3, 2, 1] resultado: " ++ show(maiorDaLista [3, 2, 1]))

    putStrLn ("")
    putStrLn ("Func. 13: entrada: [1..5] [5,4..1] resultado: " ++ show(selecionaMaiores [1..5] [5,4..1]))
    putStrLn ("Func. 13: entrada: [1..5] [15,10..0] resultado: " ++ show(selecionaMaiores [1..5] [15,10..0]))
    
    putStrLn ("")
    putStrLn ("Func. 14: entrada: (replicate 10 1) [15] resultado: " ++ show(menorSomaTotal (replicate 10 1) [15]))

    putStrLn ("")
    putStrLn ("Func. 15: entrada: \"ahbgcfde\" resultado: " ++ show(trocaCaracteresPorE "ahbgcfde"))

    putStrLn ("")
    putStrLn ("Func. 16: entrada: (10, 10) resultado: " ++ "Sem curry: " ++ show(somaTupla (10, 10)))
    putStrLn ("Func. 16: entrada: 10 10) resultado: " ++ "Com curry: " ++ show(curry somaTupla 10 10))

    putStrLn ("")
    putStrLn ("Func. 17: entrada: 5 10 resultado: " ++ "Sem uncurry: " ++ show(maiorDoPar 5 10))
    putStrLn ("Func. 17: entrada: (10, 5) resultado: " ++ "Com uncurry: " ++ show(uncurry maiorDoPar (10, 5)))
