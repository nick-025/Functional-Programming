-- Nicholas Davi da Cruz

module Main where

{-
    1. Escreva uma função para o cálculo dos números da sequência de Fibonacci, utilizando Haskell.
-}

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (+) (fibonacci (n-1)) (fibonacci (n-2))

{-
    2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum (MDC) de Euclides publicado por volta do ano 300 AC.

    Podemos simplificar este algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor absoluto de A se B = 0 e pelo MDC entre B e o resto da divisão de A por B se B > 0.

    Escreva uma função para o cálculo do MDC entre dois números inteiros positivos, usando o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
-}

-- Esta aqui é a minha função do Trabalho 2 - Introdução ao Haskell.
-- Eu não quis utilizar a função 'mod' porque ela já está pronta.
-- Esforcei-me bastante para fazer o trabalho, então quis valorizar
-- o conceito por trás da função de resto. 😅😆

calcResto :: Int -> Int -> Int -> Int
calcResto x y xsign 
    | abs x < abs y  = abs x * xsign
    | otherwise = calcResto (abs x - abs y) y xsign

resto :: Int -> Int -> Int
resto x y
    | x == 0 = 0
    | y == 0 = error "Division by zero"
    | abs x == abs y = 0
    | otherwise = calcResto x y (if x > 0 then 1 else -1)

mdc :: Int -> Int -> Int
mdc a 0 = abs a
mdc a b = mdc b (resto a b)

{-
    3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.
    Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e recursividade.
-}

somaDigitos :: Int -> Int
somaDigitos x = if x>0 then aux(abs x) else if x<0 then - aux(abs x) else 0
    where
        aux :: Int -> Int
        aux x'
            | x' < 10 = x'
            | otherwise = aux(div x' 10) + resto x' 10

{-
    4. Escreva uma função que devolva a soma de todos os números menores que 10000 que sejam múltiplos de 3 ou 5.
-}

somaMultiplosDesc :: Int -> Int
somaMultiplosDesc k = aux (k-1)
    where
        aux :: Int -> Int
        aux i
            | i < 3 = 0
            | rem i 3 == 0 || rem i 5 == 0 = i + aux (i-1)
            | otherwise = aux (i-1)

{-
    5. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.
-}

diferenca :: [Int] -> Int
diferenca [] = 0
diferenca (x:xs) = somaQuad(x:xs) - quadSoma(x:xs)^2
    where
        somaQuad :: [Int] -> Int
        somaQuad [] = 0
        somaQuad (x':xs') = x'^2 + somaQuad(xs')
        quadSoma :: [Int] -> Int
        quadSoma [] = 0
        quadSoma (x':xs') = x' + quadSoma(xs')

{-
    6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado.
-}

crivoEuler :: Int -> [Int]
crivoEuler n = peneira [2..n]
    where
        peneira :: [Int] -> [Int]
        peneira [] = []
        peneira (p:xs) = p : peneira [x | x <- xs, 0 /= resto x p]

{-
    7. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.
-}

sequenciaLucas :: Int -> [Int]
sequenciaLucas n = filter (<n) $ map nLucas [0..n]
    where
        nLucas :: Int -> Int
        nLucas 0 = 2
        nLucas 1 = 1
        nLucas n' = nLucas (n'-2) + nLucas (n'-1)

{-
    8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].
-}

aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (x:xs) = aoContrario xs ++ [x]

{-
    9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.
-}

somaRecursiva :: Int -> Int -> Int
somaRecursiva x y
    | y == 0 || x == 0 = 0
    | y > 0 =   x  + somaRecursiva x (y-1)
    | y < 0 = (-x) + somaRecursiva x (y+1)

{-
    10. Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.
-}

comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs

main :: IO()
main = do
    putStrLn ""
    putStrLn "Testes - função 1: fibonacci"
    putStrLn ("Func.  1: entrada:  0; resultado:  "++show(fibonacci 0))
    putStrLn ("Func.  1: entrada:  1; resultado:  "++show(fibonacci 1))
    putStrLn ("Func.  1: entrada: 10; resultado: "++show(fibonacci 10))

    putStrLn ""
    putStrLn "Testes - função 2: mdc"
    putStrLn ("Func.  2: entrada: 252 105; resultado: "++show(mdc 252 105))

    putStrLn ""
    putStrLn "Testes - função 3: somaDigitos"
    putStrLn ("Func.  3: entrada:   1234 ; resultado:  "++show(somaDigitos  1234))
    putStrLn ("Func.  3: entrada: (-1234); resultado: "++show(somaDigitos (-1234)))

    putStrLn ""
    putStrLn "Testes - função 4: somaMultiplosDesc"
    putStrLn ("Func.  4: entrada: 10; resultado: "++show(somaMultiplosDesc 10000))

    putStrLn ""
    putStrLn "Testes - função 5: diferenca"
    putStrLn ("Func.  5: entrada:          []; resultado:    "++show(diferenca []))
    putStrLn ("Func.  5: entrada: [1,2,3,4,5]; resultado: "++show(diferenca [1,2,3,4,5]))

    putStrLn ""
    putStrLn "Testes - função 6: crivoEuler"
    putStrLn ("Func. 6: entrada:  0; resultado:        "++show(crivoEuler 0))
    putStrLn ("Func. 6: entrada: 10; resultado: "++show(crivoEuler 10))

    putStrLn ""
    putStrLn "Testes - função 7: sequenciaLucas"
    putStrLn ("Func.  7: entrada: 7; resultado: "++show(sequenciaLucas 7))

    putStrLn ""
    putStrLn "Testes - função 8: aoContrario"
    putStrLn ("Func.  8: entrada:          []; resultado:          "++show(aoContrario []))
    putStrLn ("Func.  8: entrada: [1,2,3,4,5]; resultado: "++show(aoContrario [1,2,3,4,5]))

    putStrLn ""
    putStrLn "Testes - função 9: somaRecursiva"
    putStrLn ("Func.  9: entrada:    0    10 ; resultado:   "++show(somaRecursiva    0    10 ))
    putStrLn ("Func.  9: entrada:    0  (-10); resultado:   "++show(somaRecursiva    0  (-10)))
    putStrLn ("Func.  9: entrada:   10     0 ; resultado:   "++show(somaRecursiva   10     0 ))
    putStrLn ("Func.  9: entrada: (-10)    0 ; resultado:   "++show(somaRecursiva (-10)    0 ))
    putStrLn ("Func.  9: entrada:   10     3 ; resultado:  "++show(somaRecursiva   10     3 ))
    putStrLn ("Func.  9: entrada: (-10)    3 ; resultado: "++show(somaRecursiva (-10)    3 ))
    putStrLn ("Func.  9: entrada:   10   (-3); resultado: "++show(somaRecursiva   10   (-3)))
    putStrLn ("Func.  9: entrada: (-10)    3 ; resultado: "++show(somaRecursiva (-10)    3 ))
    putStrLn ("Func.  9: entrada:    3    10 ; resultado:  "++show(somaRecursiva    3    10 ))
    putStrLn ("Func.  9: entrada:    3  (-10); resultado: "++show(somaRecursiva    3  (-10)))
    putStrLn ("Func.  9: entrada:  (-3)   10 ; resultado: "++show(somaRecursiva  (-3)   10 ))
    putStrLn ("Func.  9: entrada:    3  (-10); resultado: "++show(somaRecursiva    3  (-10)))
    putStrLn ("Func.  9: entrada: (-10)  (-3); resultado:  "++show(somaRecursiva (-10)  (-3)))
    putStrLn ("Func.  9: entrada:  (-3) (-10); resultado:  "++show(somaRecursiva  (-3) (-10)))

    putStrLn ""
    putStrLn "Testes - função 10: comprimento"
    putStrLn ("Func. 10: entrada:          []; resultado: "++show(comprimento []))
    putStrLn ("Func. 10: entrada: [1,2,3,4,5]; resultado: "++show(comprimento [1,2,3,4,5]))
