-- File containing utility functions for lists.

module Root.Src.Exercise where

--Eu
nome = "gabriel ribeiro bernardi"   --coloque seu nome aqui, como uma String
-- juro pela minha honra, que serei ético na realização desta avaliação, 
-- não consultando nada e nem ninguém, além das notas de aulas e das 
-- minha soluções para exercícios anteriores.

{-
A função maximum procura retorna o maior elemento em uma lista.
Escreva uma função com comportamento similar, recursiva.
-}

máximo :: [Int] -> Int
máximo [] = error "lista vazia"
máximo [a] = a   -- se a lista tiver apenas um elemento, retorna ele mesmo (caso base)
máximo (cabeca : cauda)
    | cabeca > máximo cauda = cabeca -- se chegar em apenas um elemento, retorna ele
    | otherwise = máximo cauda       -- senao, faz-se a recursao


{-
A fórmula de Bhaskara permite calcular as raízes de uma equação de segundo grau
na forma ax^2 + bx + c = 0. A resolução é normalmente dividida em duas partes, o
cálculo do discriminante, Delta, e das raízes. O Delta é calculado pela equação
seguinte:
 Delta  = b^2 - 4ac

Calculado o delta, a seguinte equação calcula as raízes.
 raiz1 = (-b + Delta^(1/2)) / 2a
 raiz2 = (-b - Delta^(1/2)) / 2a

Observe que:
 Se Delta > 0, a equação do segundo grau tem 2 raízes.
 Se Delta = 0, 1 raiz.
 Se Delta < 0, tem 0 raízes reais.


1 - Escreva uma função que receba uma tripla com os coeficientes da equação,
isto é, (a,b,c), e retorne o valor de Delta.

2 - Escreva uma função que receba uma tripla com os coeficientes da equação,
isto é, (a,b,c), retorne uma lista com as raízes da equação de segundo
grau. Defina a função usando guardas. Utilize a função delta.
-}

delta :: (Float, Float, Float) -> Float
delta (a,b,c) = (b ^ 2) - (4 * a * c)

raízes :: (Float, Float, Float) -> [Float]
raízes (a,b,c)
    | delta (a,b,c) < 0 = []
    | delta (a,b,c) == 0 = [raiz1]
    | otherwise = [raiz1, raiz2]
    where 
        raiz1 = ( ( -b + ( delta (a,b,c) ** 0.5 ) ) / (2 * a) )
        raiz2 = ( ( -b - ( delta (a,b,c) ** 0.5 ) ) / (2 * a) )
    
{-
Considere que o preço de uma passagem de ônibus intermunicipal pode variar dependendo
da idade do passageiro
- crianças menos de 10 anos pagam 40% e bebês (abaixo de 2 anos) pagam apenas 15%. 
- pessoas com 70 anos ou mais pagam apenas 50% do preço total. 
- os demais passageiros pagam a tarifa normal, 100%. 

Faça uma função que tenha como entrada:
- o valor total da passagem,
- a data atual e 
- a data de nascimento do passageiro. 

Como saída, a função retorna o valor a ser pago. 

Obs. 1: na solução, deve ser definido o tipo data para representar a tupla de inteiros (d,m,a).
Obs. 2: assuma que as datas estão corretas.
Obs. 3: assuma que todos os meses tem 30 dias e o ano tem 360 dias.
-}

-- type Data = (Int, Int, Int)

-- calculaDia :: Int -> Int -> Int
-- calculaDia dia1 dia2
--     | dia1 - dia2 > 0 = 1
--     | dia1 - dia2 == 0 = 0
--     | otherwise = -1

-- calculaMes :: Int -> Int -> Int
-- calculaMes mes1 mes2 = mes1 - mes2

-- calculaAno :: Int -> Int -> Int
-- calculaAno ano1 ano2 = ano1 - ano2

-- idade :: Data -> Data -> Int
-- idade (dia1, mes1, ano1) (dia2, mes2, ano2)
--     | calculaDia dia1 dia2 == 0 && calculaMes mes1 mes2 == 0 = calculaAno ano1 ano2
--     | calculaDia dia1 dia2 == 0 = calculaMes mes1 mes2

-- valorFinal :: Float -> Data -> Data -> Float
-- valorFinal preço (dia1, mes1, ano1) (dia2, mes2, ano2)
--     | calculaIdade < 10 = 2
--     | otherwise = 2
--     where calculaIdade = idade (dia1, mes1, ano1) (dia2, mes2, ano2)
type Data = (Int, Int, Int)

calculaIdade :: Data -> Data -> Int
calculaIdade (dia1, mes1, ano1) (dia2, mes2, ano2) = ano1 - ano2

valorFinal:: Float -> Data -> Data -> Float
valorFinal preço dataNascimento dataHoje 
    | idade < 2 = preço / 100* 15
    | idade < 10 = preço / 100 * 40
    | idade >= 70 = preço / 100 * 50
    | otherwise = preço
    where idade = calculaIdade dataHoje dataNascimento



data Filtro = Menor | Maior | Igual deriving (Eq)

{-
O tipo Filtro pode ter um dos três valores definidos na linha anterior.
Escreva uma função recursiva que receba como entrada
- tupla com Filtro f na primeira posição e inteiro i na segunda posição.
- lista de inteiros l

Retorne
- Lista com todos os inteiros em l que são menores que i, se f for Menor, maiores que i se
f for Maior, e iguais a i, se f for Igual.
-}

filtre :: (Filtro,Int) -> [Int] -> [Int]
filtre (tipoFiltro, valor) l
    | tipoFiltro == Maior = dropWhile ( < valor ) l
    | tipoFiltro == Menor = takeWhile ( < valor ) l
    | tipoFiltro == Igual = filter ( == valor ) l

{-
Sabendo que:
- no mercado de ações brasileiro, ações são negociadas em lotes de 100 unidades;
- cada ação é identificada por um nome único, o "ticker", por exemplo VALE3 ou BOVA11;
- quando se compra um lote de ações, ele vai para a "carteira" do comprador;
- os proprietários das ações usam o custo médio das ações para calcular lucros e prejuízos.

Implemente as seguintes funções:
* compre
  - Entrada
     + uma tupla com o ticker (String) e o preço da ação (por unidade)
     + a quantidade de ações a comprar (múltiplo do tamanho de um lote)
     + a carteira a atual, na forma de uma lista de tuplas com ticker e custo médio das ações.
  - Retorna
     + a nova carteira, corrigida pela adição das ações compradas e com preços médios atualizados.

* venda
  - Entrada
     + uma tupla com o ticker (String) e o preço da ação (por unidade)
     + a quantidade de ações a vender (múltiplo do tamanho de um lote)
     + a carteira a atual, na forma de uma lista de tuplas com ticker e custo médio das ações.
  - Retorna
     + a nova carteira, corrigida pela remoção das ações vendidas. Se a venda não for possível,
     a carteira permanece intacta.


-}

compre :: (String, Float) -> Int -> [(String, Float, Int)] -> [(String, Float, Int)]
compre compra qtd carteira = error "Implementar"

venda venda quantidade carteira = error "Implementar" 
