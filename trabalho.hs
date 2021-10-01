-- Antonio Pedro Rodrigues Santos - 11821BCC021
-- Gabriel Ribeiro Bernardi - 11821BCC036
-- Guilherme Soares Correa - 11821BCC026

-- Criacao das informacoes dos jogadores
-- (Identificador, (Coordenadas X e Y), Direcao, (Habilidade, Quantidade))
type Jogador = ( Int, (Int, Int), String, [(Presentoso, Int)] )

-- criacao dos possiveis valores dos objetos
data Presentoso = Patins | Bomba | Arremesso deriving (Eq, Show)
data Objetos = Buraco | Grama | Pedra | Parede | Presente Presentoso | Jogador Int deriving (Show,Eq)

-- criacao das estruturas do tabuleiro
type Item = Objetos
type Celula = [Item]
type Linha = [Celula]
type Tabuleiro = [Linha]

-- criacao das celulas do tabuleiro
pedra :: Celula
pedra = [Pedra] --Parede indestrutivel

paredePresenteBomba :: Celula
paredePresenteBomba = [Parede, Presente Bomba, Grama]

paredePresentePatins :: Celula
paredePresentePatins = [Parede, Presente Patins, Grama]

paredePresenteArremesso :: Celula
paredePresenteArremesso = [Parede, Presente Arremesso, Grama]

paredeGrama :: Celula
paredeGrama = [Parede, Grama]

bomba :: Celula
bomba = [Presente Bomba, Grama]

jogador :: Celula
jogador = [Grama, Jogador 1]

grama :: Celula
grama = [Grama]

-- Criacao das linhas do tabuleiro
linha01 :: Linha
linha01 = [pedra, pedra, pedra, pedra, pedra, pedra, pedra, pedra]

linha02 :: Linha
linha02 = [pedra, grama, grama, grama, grama, grama, grama, pedra]

linha03 :: Linha
linha03 = [pedra, pedra, grama, pedra, grama, pedra, grama, pedra]

linha04 :: Linha
linha04 = [pedra, grama, grama, grama, grama, grama, grama, pedra]

linha05 :: Linha
linha05 = [pedra, pedra, grama, pedra, grama, pedra, grama, pedra]

linha06 :: Linha
linha06 = [pedra, grama, grama, grama, grama, grama, grama, pedra]

linha07 :: Linha
linha07 = [pedra, pedra, grama, pedra, grama, pedra, grama, pedra]

linha08 :: Linha
linha08 = [pedra, grama, grama, grama, grama, grama, grama, pedra]

-- criacao do tabuleiro
tabuleiro :: Tabuleiro
tabuleiro = [linha01, linha02, linha03, linha04, linha05, linha06, linha07, linha08]

-- type ElementoBase = Objetos

-- tabuleiro :: [[Objetos]] -> [[Objetos]]
-- tabuleiro s = s

-- tabuleiro = [ [ ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa ], [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], [ ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa ] ]
-- [ [ retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa ], [ retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa ], [ retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa ], [ retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa ], [ retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa ], [ retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa ], [ retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa ], [ retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa, retornaParedeFixa ] ]



-- tabuleiro = [ [ ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa ], 
--     [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], 
--     [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], 
--     [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], 
--     [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], 
--     [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], 
--     [ ParedeFixa, Grama, Grama, Grama, Grama, Grama, Grama, ParedeFixa ], 
--     [ ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa, ParedeFixa ] ]

-- configuracao :: Objetos -> String
-- configuracao obj linha coluna = let tabuleiro !! linha !! coluna = obj
