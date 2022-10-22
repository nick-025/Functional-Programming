; Nicholas Davi da Cruz

;    1. Utilizando a linguagem Clojure, crie uma função chamada ultimo que receba uma lista e devolva o último elemento desta lista sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.

(defn enesimo [leaving lista]
    (if (= (count lista) leaving)
        (first lista)
        (enesimo leaving (rest lista))
    )
)

(defn ultimo [lista] (enesimo 1 lista))

(println "Func. 1; entrada:   '(1,2,3,4,5); saída: " (ultimo '(1,2,3,4,5)) "\n")

;    2. Utilizando a linguagem Clojure, crie uma função chamada penultimo que receba uma lista e devolva o penúltimo elemento desta lista usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.

(defn penultimo [lista] (enesimo 2 lista))

(println "Func. 2; entrada:   '(1,2,3,4,5); saída: " (penultimo '(1,2,3,4,5)) "\n")

;    3. Utilizando a linguagem Clojure, crie uma função chamada elementoN que receba uma lista e um inteiro N e devolva o elemento que está na posição N desta lista usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure. 

(defn elementoN [n lista] 
    (if (= n 0)
        (first lista)
        (elementoN (- n 1) (rest lista))
    )
)

(println "Func. 3; entrada: 2 '(1,2,3,4,5); saída: " (elementoN 2 '(1,2,3,4,5)) "\n")

;    4. Utilizando a linguagem Clojure, crie uma função chamada inverso que receba uma lista e devolva esta lista com as posições dos elementos invertidas. Por exemplo recebe [1,2,3] e devolve [3,2,1]. Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.

(defn inverso [lista]
    (if (= (count lista) 0)
        []
        (conj (inverso (rest lista)) (first lista))
    )
)

(println "Func. 4; entrada: '(1,2,3,4,5); saída: " (inverso '(1,2,3,4,5)) "\n")

;    5. Utilizando a linguagem Clojure, crie uma função chamada mdc que receba dois inteiros e devolve o mínimo divisor comum entre eles. Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.

(defn mdc [a b]
    (if (= b 0)
        (Math/abs a)
        (mdc b (mod a b))
    )
)

(println "Func. 5; entrada: 252 105; saída: " (mdc 252 105))
