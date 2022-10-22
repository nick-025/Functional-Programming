; Nicholas Davi da Cruz

(defn red    [] (print "\u001B[31m"))
(defn pink   [] (print "\u001B[35m"))
(defn yellow [] (print "\u001B[93m"))
(defn green  [] (print "\u001B[32m"))
(defn blue   [] (print "\u001B[34m"))

;    1. Na aula disponível em: "https://replit.com/@frankalcantara/ClojureAula2?v=1" foram destacadas diversas funções (expressões), como funções de primeira ordem, disponíveis em Clojure. Sua primeira tarefa será descrever cada uma destas funções e apresentar dois exemplos de uso de cada uma delas. Lembre-se os exemplos precisam ser utilizados de forma que o resutado da função possa ser visto no terminal.

;    1.1  assoc
; Usada para associar um novo par de chave-valor a um mapa.

(def MAP {:1 "one" :2 "two"})

(def MAP1 (assoc MAP :3 "three"))

(red)
(println "Func. 1.1 ; entrada: assoc MAP :3 \"three\"; saída:\n" MAP1)

;    1.2  dissoc
; Usada para desassociar um par de chave-valor de um mapa.

(def MAP2 (dissoc MAP1 :1))

(println "Func. 1.2 ; entrada:       dissoc MAP1 :1; saída:\n" MAP2 "\n")

;    1.3  range
; Usada sem argumentos, cria uma lista infinita. Com dois argumentos retorna uma lista de inteiros desde o primeiro até o último valor do intervalo especificado, EXCLUSIVO NO FIM.

(pink)
(println "Func. 1.3 ; entrada:   -2 2; saída:" (range -2 3))
(println "Func. 1.3 ; entrada: take 5; saída:" (take 5 (range)) "\n")

;    1.4  map
; Mapeia uma expressão sobre uma coleção e retorna uma nova coleção transformada a partir da existente.

(defn pmod [f a] (defn pmod_ [b] (f (mod b a))))

(defn eq [a] (defn eq_ [b] (= a b)))

(defn id [a] a)

(yellow)
; Mapear os restos da divisão dos elementos de uma lista por um dado número.
(println "Func. 1.4 ; entrada: [1,2,3]; saída:"
         (map (pmod id 2) [1,2,3]))
; Mapear verdadeiro e falso para todos os restos que são ou não iguais a 0.
(println "Func. 1.4 ; entrada: [1,2,3]; saída:"
        (map (pmod (eq 0) 2) [1,2,3]) "\n")

;    1.5  inc
; Incrementa um valor numérico em 1

(green)
(println "Func. 1.5 ; entrada: 0; saída:" (inc 0))
(println "Func. 1.5 ; entrada: 0; saída:" (map inc (range 1 5)) "\n")

;    1.6  odd?
; Função que retorna verdadeiro se um número é ímpar, senão falso.

(blue)
(println "Func. 1.6 ; entrada: (range 1 5); saída:" (map odd? (range 1 5)))
(println "Func. 1.6 ; entrada: (range 1 5); saída:" (filter odd? (range 1 5)) "\n")

;    1.7  into
; Insere todos os elementos de uma coleção em outra, podendo ser vazia ou não.

(red)
(println "Func. 1.7 ; entrada: [1]   [2,3]; saída:" (into [1] [2,3]))
(println "Func. 1.7 ; entrada: [4,5] [2,3]; saída:" (into [4,5] [2,3]) "\n")

;    1.8  nth
; Retorna o enésimo elemento de uma coleção, de acordo com a posição do elemento na coleção.

(pink)
(println "Func. 1.8 ; entrada:     \"Frank\" 2; saída:" (nth "Frank" 2))
(println "Func. 1.8 ; entrada: \"Alcantara\" 0; saída:" (nth "Alcantara" 0) "\n")

;    1.9  conj
; Adiciona um elemento ao final de uma lista.

(yellow)
(println "Func. 1.9 ; entrada: [2] [3,4]; saída:" (conj [2] 3 4))
(println "Func. 1.9 ; entrada: [2] [3,4]; saída:" (conj [] 5 6 7) "\n")

;    1.10 sort
; Ordena uma coleção.

(green)
(println "Func. 1.10; entrada: \"Nicholas\"; saída:" (sort "Nicholas"))
(println "Func. 1.10; entrada:  [5,4,3,2,1]; saída:" (sort [5,4,3,2,1]) "\n")

;    1.11 partition-by
; Particiona uma coleção em coleções menores, dada uma condição.

(blue)
(println "Func. 1.11; entrada: [1,3,2,7,5]; saída:" (partition-by even? [1,3,2,7,5]))
(println "Func. 1.11; entrada: [2,6,3,5,4]; saída:" (partition-by (fn [a] (> a 2)) [2,6,3,5,4]) "\n")

;    1.12 filter
; Filtra os valores de uma coleção, criando uma nova coleção e armazenando os que atendem à condição passada como argumento.

(red)
(println "Func. 1.12; entrada: \"calaojauare\"; saída:" (filter (fn [a] (not= a (char 97))) "caalaaojauaare"))
(println "Func. 1.12; entrada: [2,6,8,4,9]; saída:" (filter (fn [a] (> a 5)) [2,6,8,4,9]) "\n")

;    1.13 empty?
; Verifica se uma coleção está vazia ou não.

(pink)
(println "Func. 1.13; entrada: \"Vazia\"; saída:" (empty? "Vazia"))
(println "Func. 1.13; entrada:      []; saída:" (empty? []) "\n")

;    1.14 count
; Retorna a contagem de elementos de uma coleção.

(yellow)
(println "Func. 1.14; entrada: \"Vazia\"; saída:" (count "Vazia"))
(println "Func. 1.14; entrada:      []; saída:" (count []) "\n")

;    1.15 char
; Retorna o caractere que corresponde ao número passado como argumento.

(green)
(println "Func. 1.14; entrada:          [70,114,97,110,107]; saída:\n" (map char [70,114,97,110,107]))
(println "Func. 1.14; entrada: [67,108,111,106,117,114,101]; saída:\n" (map char [67,108,111,106,117,114,101]) "\n")

;    2. Utilizando a linguagem Clojure, crie uma função chamada ehPrimo que receba um inteiro e devolva True caso este inteiro seja verdadeiro e False caso contrário.

(defn ehDivisor [a b] (= 0 (mod a b)))

(defn filtraDivisores [n lista]
    (if (empty? lista)
        []
        (if (ehDivisor n (first lista))
            (conj (filtraDivisores n (rest lista)) (first lista))
            (filtraDivisores n (rest lista))
        )
    )
)

(defn divisores [n] (filtraDivisores n (range 1 (- n 1))))

(defn ehPrimo [n] 
    (if (<= n 1)
        false
        (<= (count (divisores n)) 1)
    )
)

(blue)
(println "Func. 2.0 ; entrada:  5; saída:" (ehPrimo 5))
(println "Func. 2.0 ; entrada: 10; saída:" (ehPrimo 10) "\n")

;    3. Utilizando a linguagem Clojure, crie uma função chamada fatoresPrimos que receba um inteiro e devolva uma lista dos seus fatores primos. Decomposição em fatores primos é uma tarefa fundamental da aritmética.

(defn fatoresPrimos [n] (filter ehPrimo (divisores n)))

(red)
(println "Func. 3.0 ; entrada: 180; saída:" (fatoresPrimos 180))
(println "Func. 3.0 ; entrada: 363; saída:" (fatoresPrimos 363) "\n")

;    4. Utilizando a linguagem Clojure, crie uma função chamada todosPrimos que receba dois valores inteiros e devolve todos os números primos que existam entre estes dois valores.

(defn todosPrimos [a b] (filter ehPrimo (range a b)))

(pink)
(println "Func. 4.0 ; entrada:  0 20; saída:" (todosPrimos 0 20))
(println "Func. 4.0 ; entrada: 21 30; saída:" (todosPrimos 21 40) "\n")
