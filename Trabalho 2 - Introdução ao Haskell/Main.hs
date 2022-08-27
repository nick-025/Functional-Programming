-- Nicholas Davi da Cruz

module Main where

{-
    1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada. 
-}

soma1 :: Int -> Int
soma1 x = (+) x 1

{-
    2. Escreva  uma  função  chamada  'sempre'  que,  não importando  o  valor  de  entrada,  devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.
-}

sempre :: (Show anyVar) => anyVar -> Int
sempre a = 0

{-
    3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.
-}

treco :: Double -> Double -> Double -> Double
treco k i j = (*) ((+) k i) j

{-
    4. Escreva uma função chamada 'resto' que devolva o resto de uma divisão entre dois números inteiros.
-}

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

{-
    5. Escreva uma função chamada 'precoMaior' que devolva o maior valor entre quatro valores monetários.
-}

maior :: Float -> Float -> Float
maior x y
    | x > y = x
    | x < y = y
    | otherwise = x

precoMaior :: Float -> Float -> Float -> Float -> Float
precoMaior a b c d = maior (maior (maior a b ) c) d

{-
    6. Escreva uma função chamada 'impar' que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.
-}

impar :: Int -> Int -> Bool
impar x y = if not (rem ((*) x y) 2 == 0) then True else False

{-
    7. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
-}

somaPar :: (Int, Int) -> Int
somaPar (a, b) = (+) a b

{-
    8. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥^2 + 𝑦/2 +𝑧.
-}

equation :: Double -> Double -> Double -> Double
equation x y z = (/) ((+) ((*) 2 ((*) x x)) ((+) y ((*) 2 z))) 2

{-
    9. Escreva uma função em Haskell chamada 'diagnostico' que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: https://cuidadospelavida.com.br/cuidados-e-bem-estar/alimentacao/sobrepeso-obesidade-e-obesidade-morbida-entenda-diferenca.

    Observe  que  este  diagnóstico  é  meramente  estatístico  e  não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. 
Todo e qualquer diagnóstico deve ser feito por um profissional médico.  
-}

calcIMC :: Double -> Double -> Double
calcIMC w h = (/) w (h*h)

diagnostico :: Double -> Double -> String
diagnostico w h
    | calcIMC w h <  17                         = "Muito abaixo do peso"
    | calcIMC w h >= 17   && calcIMC w h < 18.5 =       "Abaixo do peso"
    | calcIMC w h >= 18.5 && calcIMC w h < 25   =          "Peso normal"
    | calcIMC w h >= 25   && calcIMC w h < 30   =            "Sobrepeso"
    | calcIMC w h >= 30   && calcIMC w h < 35   =       "Obesidade leve"
    | calcIMC w h >= 35   && calcIMC w h < 40   =     "Obesidade severa"
    | calcIMC w h >= 40                         =    "Obesidade mórbida"

{-
    10. Escreva uma função em Haskell chamada 'bissexto' que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:
    
    𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4,
        𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100,
            𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400.

1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto. 
-}

bissexto :: Int -> Bool
bissexto year
    | ((rem year 4) == 0) && ((not ((rem year 100) == 0) || ((rem year 400) == 0))) = True
    | otherwise = False

main :: IO()
main = do
    putStrLn ""
    putStrLn "Testes - função 1: soma1"
    putStrLn ("Func.  1: entrada:    9 ; resultado:  "++show(soma1 9))
    putStrLn ("Func.  1: entrada: (-11); resultado: "++show(soma1 (-11)))
    
    putStrLn ""
    putStrLn "Testes - função 2: sempre"
    putStrLn ("Func.  2: entrada:     "++show 1++"; resultado: "++show(sempre  1))
    putStrLn ("Func.  2: entrada:    "++show (-1)++"; resultado: "++show(sempre (-1)))
    putStrLn ("Func.  2: entrada:   "++show 1.1++"; resultado: "++show(sempre 1.1))
    putStrLn ("Func.  2: entrada: "++show "Str"++"; resultado: "++show(sempre "Str"))

    putStrLn ""
    putStrLn "Testes - função 3: treco"
    putStrLn ("Func.  3: entrada:  2.5  2.5  2.0"++"; resultado:  "++show(treco 2.5 2.5 2.0))
    putStrLn ("Func.  3: entrada: -2.5  2.5  2.0"++"; resultado:   "++show(treco (-2.5) 2.5 2.0))
    putStrLn ("Func.  3: entrada:  2.5 -2.5  2.0"++"; resultado:   "++show(treco 2.5 (-2.5) 2.0))
    putStrLn ("Func.  3: entrada:  2.5  2.5 -2.0"++"; resultado: "++show(treco 2.5 2.5 (-2.0)))
    putStrLn ("Func.  3: entrada: -2.5 -2.5  2.0"++"; resultado: "++show(treco (-2.5) (-2.5) 2.0))
    putStrLn ("Func.  3: entrada: -2.5  2.5 -2.0"++"; resultado:  "++show(treco (-2.5) 2.5 (-2.0)))
    putStrLn ("Func.  3: entrada:  2.5 -2.5 -2.0"++"; resultado:  "++show(treco 2.5 (-2.5) (-2.0)))
    putStrLn ("Func.  3: entrada:  2.5 -2.5 -2.0"++"; resultado:  "++show(treco (-2.5) (-2.5) (-2.0)))

    putStrLn ""

    -- IMPORTANTE !!!!!
    -- AS DUAS SEGUINTES LINHAS ESTÃO COMENTADAS PORQUE MINHA FUNÇÃO
    -- LANÇA UM ERRO QUANDO O DENOMINADOR É 0!!!
    -- NÃO QUERO ZERAR O TRABALHO PORQUE DEU UM ERRO, AINDA QUE
    -- SEJA PREVISTO E PLANEJADO PARA ACONTECER ASSIM.

    -- putStrLn ("Func. 4: entrada:   0 0"++"; resultado:  "++show(resto 0 0))
    -- putStrLn ("Func. 4: entrada:  10 0"++"; resultado:  "++show(resto 10 0))
    putStrLn "Testes - função 4: resto"
    putStrLn ("Func.  4: entrada:   0   2"++"; resultado:  "++show(resto 0 2))
    putStrLn ("Func.  4: entrada:   2  10"++"; resultado:  "++show(resto 2 10))
    putStrLn ("Func.  4: entrada:  -2  10"++"; resultado: "++show(resto (-2) 10))
    putStrLn ("Func.  4: entrada:   2 -10"++"; resultado:  "++show(resto 2 (-10)))
    putStrLn ("Func.  4: entrada:  -2 -10"++"; resultado: "++show(resto (-2) (-10)))
    putStrLn ("Func.  4: entrada:  10   2"++"; resultado:  "++show(resto 10 2))
    putStrLn ("Func.  4: entrada: -10   2"++"; resultado:  "++show(resto (-10) 2))
    putStrLn ("Func.  4: entrada:  10  -2"++"; resultado:  "++show(resto 10 (-2)))
    putStrLn ("Func.  4: entrada: -10  -2"++"; resultado:  "++show(resto (-10) (-2)))
    putStrLn ("Func.  4: entrada:  10   3"++"; resultado:  "++show(resto 10 3))
    putStrLn ("Func.  4: entrada: -10   3"++"; resultado: "++show(resto (-10) 3))
    putStrLn ("Func.  4: entrada:  10  -3"++"; resultado:  "++show(resto 10 (-3)))
    putStrLn ("Func.  4: entrada: -10  -3"++"; resultado: "++show(resto (-10) (-3)))

    -- Tantos testes assim servem para provar que o algoritmo encontra o maior
    -- valor independentemente da posição em que estiver.
    putStrLn ""
    putStrLn "Testes - função 5: precoMaior"
    putStrLn ("Func.  5: entrada: 0 1 2 3"++"; resultado: "++show(precoMaior 0 1 2 3))
    putStrLn ("Func.  5: entrada: 0 1 2 3"++"; resultado: "++show(precoMaior 0 1 2 3))
    putStrLn ("Func.  5: entrada: 0 1 3 2"++"; resultado: "++show(precoMaior 0 1 3 2))
    putStrLn ("Func.  5: entrada: 0 2 1 3"++"; resultado: "++show(precoMaior 0 2 1 3))
    putStrLn ("Func.  5: entrada: 0 2 3 1"++"; resultado: "++show(precoMaior 0 2 3 1))
    putStrLn ("Func.  5: entrada: 0 3 1 2"++"; resultado: "++show(precoMaior 0 3 1 2))
    putStrLn ("Func.  5: entrada: 0 3 2 1"++"; resultado: "++show(precoMaior 0 3 2 1))
    putStrLn ("Func.  5: entrada: 1 0 2 3"++"; resultado: "++show(precoMaior 1 0 2 3))
    putStrLn ("Func.  5: entrada: 1 0 3 2"++"; resultado: "++show(precoMaior 1 0 3 2))
    putStrLn ("Func.  5: entrada: 1 2 0 3"++"; resultado: "++show(precoMaior 1 2 0 3))
    putStrLn ("Func.  5: entrada: 1 2 3 0"++"; resultado: "++show(precoMaior 1 2 3 0))
    putStrLn ("Func.  5: entrada: 1 3 0 2"++"; resultado: "++show(precoMaior 1 3 0 2))
    putStrLn ("Func.  5: entrada: 1 3 2 0"++"; resultado: "++show(precoMaior 1 3 2 0))
    putStrLn ("Func.  5: entrada: 2 0 1 3"++"; resultado: "++show(precoMaior 2 0 1 3))
    putStrLn ("Func.  5: entrada: 2 0 3 1"++"; resultado: "++show(precoMaior 2 0 3 1))
    putStrLn ("Func.  5: entrada: 2 1 0 3"++"; resultado: "++show(precoMaior 2 1 0 3))
    putStrLn ("Func.  5: entrada: 2 1 3 0"++"; resultado: "++show(precoMaior 2 1 3 0))
    putStrLn ("Func.  5: entrada: 2 3 0 1"++"; resultado: "++show(precoMaior 2 3 0 1))
    putStrLn ("Func.  5: entrada: 2 3 1 0"++"; resultado: "++show(precoMaior 2 3 1 0))
    putStrLn ("Func.  5: entrada: 3 0 1 2"++"; resultado: "++show(precoMaior 3 0 1 2))
    putStrLn ("Func.  5: entrada: 3 0 2 1"++"; resultado: "++show(precoMaior 3 0 2 1))
    putStrLn ("Func.  5: entrada: 3 1 0 2"++"; resultado: "++show(precoMaior 3 1 0 2))
    putStrLn ("Func.  5: entrada: 3 1 2 0"++"; resultado: "++show(precoMaior 3 1 2 0))
    putStrLn ("Func.  5: entrada: 3 2 0 1"++"; resultado: "++show(precoMaior 3 2 0 1))
    putStrLn ("Func.  5: entrada: 3 2 1 0"++"; resultado: "++show(precoMaior 3 2 1 0))

    putStrLn ""
    putStrLn "Testes - função 6: impar"
    putStrLn ("Func.  6: entrada:  3  2"++"; resultado: "++show(impar  3  2))
    putStrLn ("Func.  6: entrada:  3  3"++"; resultado:  "++show(impar  3  3))
    putStrLn ("Func.  6: entrada: -3  3"++"; resultado:  "++show(impar (-3)  3))
    putStrLn ("Func.  6: entrada:  3 -3"++"; resultado:  "++show(impar  3 (-3)))
    
    putStrLn ""
    putStrLn "Testes - função 7: somaPar"
    putStrLn ("Func.  7: entrada: ( 10,  10)"++"; resultado:  "++show(somaPar ( 10,  10)))
    putStrLn ("Func.  7: entrada: ( 10, -10)"++"; resultado:   "++show(somaPar ( 10, (-10))))
    putStrLn ("Func.  7: entrada: (-10,  10)"++"; resultado:   "++show(somaPar ((-10),  10)))
    putStrLn ("Func.  7: entrada: (-10, -10)"++"; resultado: "++show(somaPar ((-10), (-10))))
    
    putStrLn ""
    putStrLn "Testes - função 8: equation"
    putStrLn ("Func.  8: entrada:   2  2  2"++"; resultado: "++show(equation  2  2  2))
    putStrLn ("Func.  8: entrada:  -2  2  2"++"; resultado: "++show(equation (-2)  2  2))
    putStrLn ("Func.  8: entrada:   2 -2  2"++"; resultado: "++show(equation  2 (-2)  2))
    putStrLn ("Func.  8: entrada:   2  2 -2"++"; resultado: "++show(equation  2  2 (-2)))
    putStrLn ("Func.  8: entrada:  -2 -2  2"++"; resultado: "++show(equation (-2) (-2)  2))
    putStrLn ("Func.  8: entrada:  -2  2 -2"++"; resultado: "++show(equation (-2)  2 (-2)))
    putStrLn ("Func.  8: entrada:   2 -2 -2"++"; resultado: "++show(equation  2 (-2) (-2)))
    putStrLn ("Func.  8: entrada:  -2 -2 -2"++"; resultado: "++show(equation (-2) (-2) (-2)))

    -- IMPORTANTE !!!!!
    -- NÃO HAVIA INFORMAÇÃO SOBRE A ALTURA QUE DEVE SER PASSADA COMO ENTRADA NA FUNÇÃO.
    -- ENTÃO, ESTOU SEGUINDO À RISCA O QUE TEMOS DISPONÍVEL NO SITE : ALTURA -> 1.72
    -- DESSA FORMA, VOU VARIAR A MASSA PARA TER RESULTADOS EM TODO O ALCANCE DOS INTERVALOS
    putStrLn ""
    putStrLn "Testes - função 9: diagnostico"
    putStrLn ("Func. 9: entrada:  50 1.72"++"; resultado: "++diagnostico 50 1.72)
    putStrLn ("Func. 9: entrada:  60 1.72"++"; resultado:          "++diagnostico 60 1.72)
    putStrLn ("Func. 9: entrada:  70 1.72"++"; resultado:          "++diagnostico 70 1.72)
    putStrLn ("Func. 9: entrada:  80 1.72"++"; resultado:            "++diagnostico 80 1.72)
    putStrLn ("Func. 9: entrada:  90 1.72"++"; resultado:       "++diagnostico 90 1.72)
    putStrLn ("Func. 9: entrada: 100 1.72"++"; resultado:       "++diagnostico 100 1.72)
    putStrLn ("Func. 9: entrada: 110 1.72"++"; resultado:     "++diagnostico 110 1.72)
    putStrLn ("Func. 9: entrada: 120 1.72"++"; resultado:    "++diagnostico 120 1.72)

    putStrLn ""
    putStrLn "Testes - função 10: bissexto"
    putStrLn ("Func. 10: entrada:  1997"++"; resultado: "++show(bissexto 1997))
    putStrLn ("Func. 10: entrada:  1990"++"; resultado: "++show(bissexto 1990))
    putStrLn ("Func. 10: entrada:  2000"++"; resultado:  "++show(bissexto 2000))