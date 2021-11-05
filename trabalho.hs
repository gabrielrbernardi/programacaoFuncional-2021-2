-- Antonio Pedro Rodrigues Santos - 11821BCC021
-- Gabriel Ribeiro Bernardi - 11821BCC036
-- Guilherme Soares Correa - 11821BCC026

import System.Random

--para gerar um tabuleiro, utilize a funcao a seguir e guarde o valor gerado.
{-
>>>criaTabuleiro
-}

-- para verificar o que ha em uma determinada celula do tabuleiro, utilize algo similar a:
{-
>>> retornaValorCelula (criaTabuleiro) 1 1
[Player_1,Grama]
-}
-- Para o criaTabuleiro, substitua pelo tabuleiro gerado na funcao criaTabuleiro. valores diferentes podem ser encontrados caso utilize tabuleiros diferentes

type Celula = [Objetos]
type Linha = [Celula]
type Tabuleiro = [Linha]
type Jogador = ( Int, (Int, Int), Coordenadas, ((Objetos, Int), (Objetos, Int), (Objetos, Int)) )

data Objetos = Grama | Parede | Pedra | Bomba | Arremesso | Patins | Fogo | Player_1 | Player_2 | Player_3  deriving (Show,Eq,Read)
data Coordenadas = L | O | N | S deriving (Eq,Show)

{-
    Definicao das regras
-}

presentoso :: Objetos -> Char
presentoso presente
    | presente == Arremesso = 'A'
    | presente == Patins = 'P'
    | presente == Fogo = 'F'

regrasSobreposicao :: Celula -> Bool   
regrasSobreposicao lista@(cabeca:resto)
    | null lista = True
    | cabeca == Grama = True
    | cabeca == Parede = True
    | cabeca == Pedra = True
    | (cabeca == Pedra || cabeca == Grama) && null resto = True
    | (cabeca == Player_1 || cabeca == Player_2 || cabeca == Player_3) && (head resto) == Grama = True
    | (cabeca == Bomba || cabeca == Fogo || cabeca == Arremesso || cabeca == Patins) && (head resto) == Grama = True
    | cabeca == Parede && (null resto || (head resto) == Arremesso || (head resto) == Grama || (head resto) == Patins || (head resto) == Fogo) = regrasSobreposicao resto
    | otherwise = False

{-
    Retorno e verificacao de valores para a geracao do jogo
-}

retornaValorJogador :: Objetos -> Int
retornaValorJogador objetos
   | objetos == Player_1 = 1
   | objetos == Player_2 = 2
   | objetos == Player_3 = 3

confereJogador :: Objetos -> Bool 
confereJogador j
    | j == Player_1 = True
    | j == Player_2 = True
    | j == Player_3 = True
    | otherwise = False

retornaJogador :: Objetos -> [Jogador] -> Jogador
retornaJogador jogador listaJogador = last (take (retornaValorJogador jogador) listaJogador)

retornaJogadores :: Tabuleiro -> Int -> [Jogador]
retornaJogadores lista@(cabeca:resto) valor 
    | null lista = []
    | otherwise = (comecaJogoJogadores cabeca valor 0) ++ retornaJogadores resto (valor + 1)

{-
    Preparativos para inicializacao do jogo
-}

retornaValorCelula :: Tabuleiro -> Int -> Int -> Celula
retornaValorCelula tabuleiro linha coluna
        | linha == 0 && coluna == 0 = head (head tabuleiro)  
        | linha > 0 && coluna == 0 = head (last (take (linha + 1) tabuleiro))
        | linha == 0 && coluna > 0 = last (take (coluna + 1) (head tabuleiro))
        | otherwise = last (take (coluna+1) (last (take (linha + 1) tabuleiro)))

comecaJogoJogadores :: Linha -> Int -> Int -> [Jogador]
comecaJogoJogadores lista@(cabeca:resto) linha coluna
    | null lista = []
    | null cabeca = comecaJogoJogadores resto linha (coluna + 1)
    | confereJogador (head cabeca) == False = comecaJogoJogadores resto linha (coluna + 1)
    | otherwise = (retornaValorJogador (head cabeca), (linha, coluna), N, ((Patins, 0), (Fogo, 0), (Arremesso, 0))) : comecaJogoJogadores resto linha (coluna + 1)


{-
    Criando e administrando a movimentoacao do jogador
-}

verificaMovimentacao :: Tabuleiro -> Int -> Int -> Bool
verificaMovimentacao tabuleiro linha coluna = verificaAcessoCelula tabuleiro linha coluna

verificaAcessoCelula :: Tabuleiro -> Int -> Int -> Bool
verificaAcessoCelula tabuleiro linha coluna
    | head (retornaValorCelula tabuleiro linha coluna) == Pedra || head (retornaValorCelula tabuleiro linha coluna) == Parede || head (retornaValorCelula tabuleiro linha coluna) == Bomba = False
    | otherwise = True

movimentaJogador :: Tabuleiro -> Jogador -> Coordenadas -> Jogador
movimentaJogador t (jogadorId, (linha, coluna), coordenadaAtual, ((Patins, qtdPatins), (Fogo, qtdFogo), (Arremesso, qtdArremesso))) coordenadaDesejada
        | coordenadaDesejada == L && coordenadaAtual == coordenadaDesejada && verificaMovimentacao t linha (coluna+1) = (jogadorId, (linha, coluna+1), coordenadaAtual, ((Patins, qtdPatins), (Fogo, qtdFogo), (Arremesso, qtdArremesso)))
        | coordenadaDesejada == O && coordenadaAtual == coordenadaDesejada && verificaMovimentacao t linha (coluna-1) = (jogadorId, (linha, coluna-1), coordenadaAtual, ((Patins, qtdPatins), (Fogo, qtdFogo), (Arremesso, qtdArremesso)))
        | coordenadaDesejada == N && coordenadaAtual == coordenadaDesejada && verificaMovimentacao t (linha-1) coluna = (jogadorId, (linha-1, coluna), coordenadaAtual, ((Patins, qtdPatins), (Fogo, qtdFogo), (Arremesso, qtdArremesso)))
        | coordenadaDesejada == S && coordenadaAtual == coordenadaDesejada && verificaMovimentacao t (linha+1) coluna = (jogadorId, (linha+1, coluna), coordenadaAtual, ((Patins, qtdPatins), (Fogo, qtdFogo), (Arremesso, qtdArremesso)))
        | otherwise = (jogadorId, (linha, coluna), coordenadaDesejada, ((Patins, qtdPatins), (Fogo, qtdFogo), (Arremesso, qtdArremesso)))

{-
    Criacao do tabuleiro e seus componentes
-}

criaLinha :: Int -> Int -> Int -> [Int] -> Linha
criaLinha linha coluna valor listaAleatoria
    | linha == 0 && coluna == 0 = [Pedra] : criaLinha linha (coluna+1) valor listaAleatoria
    -- posiciona jogadores no tabuleiro
    | linha == 1 && coluna == 1 = [Player_1, Grama] : criaLinha linha (coluna+1) valor listaAleatoria
    | linha == 3 && coluna == 3 = [Player_2, Grama] : criaLinha linha (coluna+1) valor listaAleatoria
    | linha == 6 && coluna == 6 = [Player_3, Grama] : criaLinha linha (coluna+1) valor listaAleatoria
    -- gerando segunda linha
    | linha == 1 && coluna < (valor-1) = [Grama] : criaLinha linha (coluna+1) valor listaAleatoria
    | coluna == 0 && linha /= valor = [Pedra] : criaLinha linha (coluna+1) valor listaAleatoria
    -- para as linhas pares, havera blocos de pedra no meio da linha
    | even linha && even coluna && coluna /= valor  = [Pedra] : criaLinha linha (coluna+1) valor listaAleatoria
    -- ultimas linhas
    | (linha == 0 || linha == valor) && coluna < valor = [Pedra] : criaLinha linha (coluna+1) valor listaAleatoria
    | coluna == valor && linha <= valor = [Pedra] : []
    -- caso contrario
    | otherwise = substituiObjetos (last (take coluna listaAleatoria)) : criaLinha linha (coluna+1) valor listaAleatoria 

-- gerando lista com 9 valores pseudo-aleatorios e colocando-os em uma linha
linhaTabuleiro :: Int -> Linha
linhaTabuleiro linha = criaLinha linha 0 7 (take 9 (randomRs (1,10) (mkStdGen 2)))

criaTabuleiro :: Tabuleiro
criaTabuleiro = [linhaTabuleiro 0,
                 linhaTabuleiro 1,
                 linhaTabuleiro 2,
                 linhaTabuleiro 3,
                 linhaTabuleiro 4,
                 linhaTabuleiro 5,
                 linhaTabuleiro 6,
                 linhaTabuleiro 7]               

substituiObjetos :: Int -> Celula
substituiObjetos valor
    | valor == 1 = [Parede]
    | valor == 2 = [Grama]
    | valor == 3 = [Parede,Grama]
    | valor == 5 = [Patins,Grama]
    | valor == 6 = [Arremesso,Grama]
    | valor == 7 = [Fogo,Grama]
    | valor == 8 = [Parede,Patins,Grama]
    | valor == 9 = [Parede,Arremesso,Grama]
    | valor == 10 = [Parede,Fogo,Grama]
