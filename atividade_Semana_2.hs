module Root.Src.Exercise where

-- Defina as seguintes funções usando equações simples.

{-
Esta função simplesmente retorna uma string com seu nome (ou qualquer outra string, na verdade).

digaMeuNome 
- Entrada: nada
- Saída: String

>>>digaMeuNome
"Meu Nome"
-}
digaMeuNome = "Meu Nome"

{-
Esta função retorna a soma de 3 números.

somaTresNumeros 
- Entrada: n1, n2, n3
- Saída: n1 + n2 + n3

>>>somaTresNumeros 1 2 3
6

>>>somaTresNumeros 100 200 300
600
-}
somaTresNumeros n1 n2 n3 = n1 + n2 + n3

{-
Esta função retorna a área do quadrado de lado l

areaQuadrado
- Entrada: l
- Saída: l**2

Dica: ** é o operador de potenciação

>>>areaQuadrado 2
4.0

>>>areaQuadrado 3
9.0
-}
areaQuadrado l = l * l

{-
Esta função retorna a área do retângulo de lado l1 e l2

areaRetangulo
- Entrada: l1 e l2
- Saída: l1 * l2

>>>areaRetangulo 3 4

-}
areaRetangulo l1 l2 = l1*l2

{-
Esta função retorna a área do círculo de raio r

areaCirculo
- Entrada: r
- Saída: r**2 * pi

Dica: pi é uma constante disponível no módulo prelude

>>>areaCirculo 3
28.274333882308138
-}
areaCirculo r = pi * r * r


{-
Esta função retorna a área do triângulo de base b e altura h

areaTriangulo
- Entrada: b h
- Saída: b*h/2

>>>areaTriangulo 4 4
-}
areaTriangulo a h = a * h / 2


{-
Esta função retorna a área do trapézio de bases b1 e b2 e altura h

areaTriangulo
- Entrada: b1 b2 h
- Saída: b1*b2/2 * h

>>>areaTrapezio 2 4 3
9.0
-}
areaTrapezio b1 b2 h = ((b1 + b2) * h) /2


{-
Esta função retorna a hipotenusa do triângulo retângulo de lados b c

hipotenusa
- Entrada: b c
- Saída: sqrt (b*b + c*c)

Dica: sqrt retorna a raiz quadrada de um número

>>>hipotenusa 3 4
5.0
-}
hipotenusa b c = sqrt (b*b + c*c)

{-
Escreva uma função de conversão de Célcius para Fahrenheit
>>>celsius2fahrenheit 0
>>>celsius2fahrenheit 100
32.0
212.0
-}
celsius2fahrenheit c = (c * 9/5) + 32

{-
Escreva uma função de conversão de Fahrenheit para Célcius

>>>fahrenheit2celsius 212
100.0

>>>fahrenheit2celsius 32
0.0

-}
fahrenheit2celsius f = (f-32) * 5/9

-- Defina as seguintes funções usando if then else
{-
Esta função retorna o maior de tres números

maiorDeTres
- Entrada: a b c
- Saída: o maior dentre a b c

>>>maiorDeTres 1 2 3
3

>>>maiorDeTres 3 2 1
3

>>>maiorDeTres 1 3 1
3

>>>maiorDeTres (-1) (-2) (-3)
-1
-}
maiorDeTres a b c 
    | a > b && a > c = a
    | b > a && b > c = b
    | c > a && c > b = c

{-
Esta função indica se os números passados como parâmetro estão ordenados de forma decrescente

estaoOrdenados
- Entrada: a b c
- Saída: True se a,b,c estiverem ordenados e False caso contrário.

>>>estaoOrdenados 1 2 3
False

>>>estaoOrdenados 3 2 1
True

>>>estaoOrdenados 1 3 2
False
-}
estaoOrdenados a b c 
    | a > b && b > c = True
    | otherwise = False

{-
Esta função calcula o preço com desconto dos itens caso um número mínimo tenha sido comprado.

precoComDesconto
- Entrada: 
    - precoUnitario: preço de um item
    - quantidade
    - limite: quantidade de itens que se ultrapassada levam a desconto no preço
    - desconto: valor do desconto a ser aplicado, entre 0 e 1, por exemplo, 0.1 para 10% de desconto.
- Saída: Valor com o desconto

>>>precoComDesconto 1 1 3 0.1
1.0

>>>precoComDesconto 1 3 3 0.1
3.0

>>>precoComDesconto 1 4 3 0.1
3.6
-}
precoComDesconto preco quantidade limite desconto
    | quantidade > limite = preco * quantidade * (1 - desconto)
    | otherwise = preco * quantidade

{-
Esta função calcula calcula se o primeiro valor ganha do segundo em um jogo de pedra/tesoura/papel

pedraTesouraPapel
- Entrada: m1 m2 - 0 é pedra, 1 é tesoura, 2 é papel.
- Saída: True se m1 ganha e False se m2 ganha

>>>pedraTesouraPapel 0 1
True

>>>pedraTesouraPapel 1 2
True

>>>pedraTesouraPapel 2 0
True

>>>pedraTesouraPapel 1 0
False

>>>pedraTesouraPapel 2 1
False

>>>pedraTesouraPapel 0 2
False
-}
pedraTesouraPapel m1 m2
    | m1 == 0 && m2 == 1 = True
    | m1 == 1 && m2 == 2 = True
    | m1 == 2 && m2 == 0 = True
    | m1 == 1 && m2 == 0 = False
    | m1 == 2 && m2 == 1 = False
    | m1 == 0 && m2 == 2 = False
    | m1 == m2 = False
    | otherwise = False

-- Defina as seguintes funções usando guardas

{-
Esta função calcula calcula se o primeiro valor ganha do segundo em um jogo de pedra/tesoura/papel

pedraTesouraPapelGuardas
- Entrada: m1 m2 - 0 é pedra, 1 é tesoura, 2 é papel.
- Saída: True se m1 ganha e False se m2 ganha

>>>pedraTesouraPapelGuardas 0 1
True

>>>pedraTesouraPapelGuardas 1 2
True

>>>pedraTesouraPapelGuardas 2 0
True

>>>pedraTesouraPapelGuardas 1 0
False

>>>pedraTesouraPapelGuardas 2 1
False

>>>pedraTesouraPapelGuardas 0 2
False
-}
pedraTesouraPapelGuardas m1 m2
    | m1 == 0 && m2 == 1 = True
    | m1 == 1 && m2 == 2 = True
    | m1 == 2 && m2 == 0 = True
    | m1 == 1 && m2 == 0 = False
    | m1 == 2 && m2 == 1 = False
    | m1 == 0 && m2 == 2 = False
    | m1 == m2 = False
    | otherwise = False

{-
Esta função retorna -1 se os números passados como parâmetro estão em ordem decrescente, 1 ordem crescente, e 0 caso contrário.
- Entrada: x y z
- Saída: -1, x >= y >= z, 1 se x <= y <= z, 0 caso contrário.

>>>sobeDesceBagunca 1 2 3
1

>>>sobeDesceBagunca 1 3 2
0

>>>sobeDesceBagunca 3 2 1
-1
-}
sobeDesceBagunca x y z
    | x >= y && y >= z = -1
    | x <= y && y <= z = 1
    | otherwise = 0


{-
Esta função retorna a quantidade de dias no mês indicado no parâmetro. Assuma que fevereiro tem 28 dias.
- Entrada: mes - número
- Saída: Dias no mes

>>>diasMes 1
31

>>>diasMes 2
28

>>>diasMes 4
30

-}
diasMes m
    | m == 1 = 31
    | m == 2 = 28
    | m == 3 = 31
    | m == 4 = 30
    | m == 5 = 31
    | m == 6 = 30
    | m == 7 = 31
    | m == 8 = 31
    | m == 9 = 30
    | m == 10 = 31
    | m == 11 = 30
    | m == 12 = 31
    