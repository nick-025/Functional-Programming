# Nicholas Davi da Cruz

# 1. Usando os conceitos de programação funcional e considerando o universo dos números inteiros, implemente a função foldr em Python, C ou C++ 20 tomando como base o funcionamento desta função em Haskell.
# Sem, é claro, usar qualquer função, objeto, ou biblioteca disponíveis na linguagem que você escolher.

def add(a, b): return a + b
def sub(a, b): return a - b
def mul(a, b): return a * b
def div(a, b): return a / b

# Assinatura em Haskell:
# foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
# Interpretação: foldr é uma função que recebe
# uma função binária
# um valor base
# um objeto iterável
def foldr(func, value, l):
    if not l:
        return value
    return func(l[0], foldr(func, value, l[1:]))

# Função extra de contagem utilizando 'foldr'
def count(elm, iterable):
    return foldr(lambda a, b: b + 1 if elm == a else b, 0, iterable)

# Essa é pra você, professor
print(foldr(add,'',['P','r','o','f','e','s','s','o','r',' ','F','r','a','n','k',',',' ','o','l','h','a',' ','s','ó',':','\n']))
print(f"Func. 1.1: entrada: [ 1, 2, 3, 4, 5]; resultado:  {foldr(add,0,[1,2,3,4,5])}")
print(f"Func. 1.2: entrada: [ 1, 2, 3, 4, 5]; resultado:   {foldr(sub,0,[1,2,3,4,5])}")
print(f"Func. 1.3: entrada: [ 1, 2, 3, 4, 5]; resultado: {foldr(mul,1,[1,2,3,4,5])}")
print(f"Func. 1.4: entrada: [16, 8, 4, 2   ]; resultado: {foldr(div,1,[16,8,4,2])}")

print()
print(f"Func. 1.5: entrada: 'a' 'bbbb'; resultado: {count('a','bbbb')}")
print(f"Func. 1.5: entrada: 'a' 'bbba'; resultado: {count('a','bbba')}")
print(f"Func. 1.5: entrada: 'a' 'aaaa'; resultado: {count('a','aaaa')}")

# 2. Usando os conceitos de programação funcional e considerando o universo dos números inteiros, implemente a função abs em Python, C ou C++ 20 que devolva o valor absoluto de um número dado.
# Sem, é claro, usar qualquer função, objeto, ou biblioteca disponíveis na linguagem que você escolher.

def abs(num): return num if num >= 0 else -num

print()
print(f"Func. 2: entrada:  10; resultado: {abs(10)}")
print(f"Func. 2: entrada:   0; resultado:  {abs(0)}")
print(f"Func. 2: entrada: -10; resultado: {abs(-10)}")

# 3. Usando os conceitos de programação funcional e considerando o universo dos números reais, implemente a função média em Python, C ou C++ 20 que devolva a média aritmética entre dois números dados.
# Sem, é claro, usar qualquer função, objeto, ou biblioteca disponíveis na linguagem que você escolher.

def mean(a, b): return (a + b)/2

print()
print(f"Func. 3: entrada:  9,11; resultado: {mean(9,11)}")
print(f"Func. 3: entrada: -3, 5; resultado:  {mean(-3,5)}")

# 4. Usando os conceitos de programação funcional e a linguagem Python, C ou C++ 20 escreva uma função que devolva a lista de todos os números de Fibonacci até um valor dado considerando que a sequência de Fibonacci começa em zero.
# Sem, é claro, usar qualquer função, objeto, ou biblioteca disponíveis na linguagem que você escolher.

def fib(n):
    return n if n in {0,1} else fib(n-1) + fib(n-2)

# Uma possível abordagem em Haskell: concatenação de listas.
def n_fib_numbers(n):
    def recurse(_n):
        return [] if _n < 0 else recurse(_n-1) + [fib(_n)]
    return recurse(n-1)

# Conseguindo os números de fibonacci menores do que 'n'
# Higher Order Less than.
less_than = lambda a: lambda b: b < a

# Minha própria função de filtro.
def _filter(func, _list):
    if not _list:
        return []
    if func(_list[0]):
        return [_list[0]] + _filter(func, _list[1:])
    return _filter(func, _list[1:])

# Não gostei da abordagem abaixo, pois, calcula-se todos os números
# desnecessariamente dentro da list comprehension, para só então filtrá-los.

# def less_than_n_fibonacci(n):
#     return _filter(less_than(n), [fib(i) for i in range(n+1)])

# A ideia abaixo é BEM mais enxuta e calcula só o que é necessário
# para obter todos os números de fibonacci menores que 'n'
def less_than_n_fibonacci(n):
    def recurse(i):
        num = fib(i)
        if num >= n:
            return []
        return [num, *recurse(i+1)]
    return recurse(0)

print()
print(f"Func. 4: entrada:  0; resultado: {less_than_n_fibonacci(0)}")
print(f"Func. 4: entrada:  1; resultado: {less_than_n_fibonacci(1)}")
print(f"Func. 4: entrada:  2; resultado: {less_than_n_fibonacci(2)}")
print(f"Func. 4: entrada:  4; resultado: {less_than_n_fibonacci(4)}")
print(f"Func. 4: entrada:  8; resultado: {less_than_n_fibonacci(8)}")
print(f"Func. 4: entrada: 16; resultado: {less_than_n_fibonacci(16)}")

# 5. Você tem uma lista de tuplas onde o primeiro campo é o nome de um aluno e o segundo sua nota. Crie uma função, usando o Python, C ou C++ 20 e os conceitos de programação funcional para criar uma função que ordene listas de tuplas, como a tupla descrita neste enunciado.
# Sem, é claro, usar qualquer função, objeto, ou biblioteca disponíveis na linguagem que você escolher.

students = [('Carlos',     9.7),
            ('Alice',     10.0),
            ('Carla',      9.2),
            ('Bárbara',    8.5),
            ('Alexandre', 10.0)]

def is_sorted(iterable):
    if len(iterable) in {0,1}:
        return True
    if iterable[0] <= iterable[1]:
        return is_sorted(iterable[1:])
    return False

def swap(a, b, _list):
    _list[a], _list[b] = _list[b], _list[a]

def sort_students(_list):
    def sort(a, b):
        if b == len(_list):
            return sort(0, 1)
        if is_sorted(_list):
            return _list
        if _list[a][0] > _list[b][0]:
            swap(a, b, _list)
        return sort(a+1,b+1)
    return sort(0, 1)

print('\nLista de estudantes antes da ordenação:\n')
print(*['{:<9} : {:>4.1f}'.format(*i) for i in students], sep='\n')

sort_students(students)

print('\nLista de estudantes após a ordenação:\n')
print(*['{:<9} : {:>4.1f}'.format(*i) for i in students], sep='\n')
