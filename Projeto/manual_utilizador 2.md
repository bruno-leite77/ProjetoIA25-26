# Projeto Nº2: Época Normal

## Inteligência Artificial 2025/2026

**Docentes:** Prof. Joaquim Filipe, Eng. Filipe Mariano

# Jogo do Solitário 2

## Manual de Utilizador

**Realizado por:**

* António Guerreiro (202200160)
* Bruno Leite (202100504)
* Guilherme Cruz (2024129841)

**Data:** 12 de Dezembro de 2025

---

# Índice

1. Introdução
2. Instalação e Utilização
3. Configuração
4. Interface da Aplicação
5. Input/Output

---

# 1. Introdução

No âmbito da unidade curricular de Inteligência Artificial, este projeto aborda a resolução do **Jogo do Solitário** em duas vertentes:

**Fase 1 (Solitário 1):** Resolução automática de puzzles de um jogador, utilizando métodos de procura em espaço de estados (BFS, DFS, A*). O objetivo é reduzir o número de peças no tabuleiro a apenas uma.

**Fase 2 (Solitário 2):** Jogo para dois jogadores (Humano vs Computador ou Computador vs Computador). O objetivo é ser o primeiro a colocar uma peça numa das casas inicialmente ocupadas pelo adversário, utilizando o algoritmo **Negamax com cortes Alfa-Beta**.

# 2. Instalação e Utilização

O projeto foi desenvolvido em linguagem Common Lisp com recurso ao ambiente de desenvolvimento **LispWorks**.

### Requisitos do Sistema

Para a correta execução do software, é necessário garantir que os seguintes ficheiros se encontram na mesma diretoria:

* `projeto.lisp`: Ficheiro principal contendo o menu, a gestão de ficheiros e a interação com o utilizador.
* `puzzle.lisp`: Módulo do domínio contendo a definição do tabuleiro, peças, operadores, e heurísticas.
* `procura.lisp`: Módulo genérico contendo a implementação dos algoritmos de procura (BFS, DFS, A*) e métricas.
* `problemas.dat`: Ficheiro de dados contendo as configurações iniciais dos tabuleiros a resolver.
* `jogo.lisp`: Carrega os outros ficheiros de código, escreve e lê de ficheiros e trata da interação com o utilizador.

# 3. Configuração

Para iniciar o sistema, siga os passos abaixo no LispWorks:

1. **Iniciar o LispWorks** e abrir o ficheiro `projeto.lisp`.

2. **Definir o Diretório de Trabalho:**
    Antes de compilar, é necessário indicar ao LispWorks onde estão os ficheiros para que as dependências sejam carregadas corretamente. Execute o seguinte comando no *Listener*, ajustando o caminho para a sua pasta local:

    ```lisp
    (hcl:change-directory "C:\\Caminho\\Para\\O\\Vosso\\Projeto\\")
    ```

3. **Carregar o Programa:**
    Compile e carregue o buffer do ficheiro `projeto.lisp`. O código contém instruções para carregar automaticamente os módulos dependentes:

    ```lisp
    (compile-file "puzzle.lisp")
    (load "puzzle")
    (compile-file "procura.lisp")
    (load "procura")
    ```

4. **Iniciar a Interface:**
    Para lançar o menu principal, execute a função no *Listener*:

    ```lisp
    (menu-principal)
    ```

# 4. Interface da aplicação

A interação com a aplicação é realizada através da consola (Listener).

## 4.1. Menu Principal

Ao iniciar, é apresentado o menu principal com duas opções. O utilizador deve introduzir o número da opção desejada seguido da tecla *Enter*.

    ```text 
            ===============================================
                    PROJETO SOLITARIO - IA 2025/2026
            ===============================================

            1. Fase 1: Resolver Problema (1 Jogador)
            2. Fase 2: Multijogador (2 Jogadores)
            3. Sair
            Opcao >
    ```

Se for a primeira execução, o sistema carregará automaticamente a lista de problemas a partir do ficheiro `problemas.dat`.

# 5. Modos de Operação

## 5.1. Fase 1: Resolução de Problemas

Neste modo, o utilizador escolhe um tabuleiro do ficheiro `problemas.dat` e um algoritmo:

* `BFS (Largura)`: Garante a solução ótima.
* `DFS (Profundidade)`: Requer limite de profundidade.
* `A (Heurísticas)`: Base (Mobilidade) ou Extra (Contagem de Pinos).

## 5.2. Fase 2: Multijogador

Este modo permite disputar partidas multijogador no tabuleiro.

### Modos de Jogo

1. **1. Humano vs Computador:** O utilizador joga contra o computador. É necessário definir quem joga com as peças de Cima (Jogador 1) e o tempo limite para o computador pensar.

2. **2. Computador vs Computador:** Dois computadores jogam entre si. Apenas é necessário definir o tempo limite por jogada.

### Regras de Abertura

1. **1. Jogador 1 (Peças do Topo) :** Na sua primeira jogada, é obrigado a mover uma peça da 2.ª linha para baixo (movimento b).

2. **2. Jogador 2 (Peças do Fundo) :** Na sua primeira jogada, é obrigado a mover uma peça da 6.ª linha para cima (movimento c).

### Critério de Vitória

Vence o jogador que conseguir colocar uma das suas peças numa das casas de partida do adversário (linhas 1 e 2 para o Jogador 1; linhas 6 e 7 para o Jogador 2).

# 6. Input/Output

## 6.1. Inserir Jogadas (Humano)

Quando for o seu turno, deve introduzir a jogada no formato de lista (Operador Linha Coluna).

### Operadores Disponíveis

| Tipo | Código | Descrição | Exemplo |
| :--- | :---: | :--- | :--- |
| **Movimento Simples** | `d` | Move uma peça para a direita | `(d 4 2)` |
| | `e` | Move uma peça para a esquerda | `(e 4 3)` |
| | `c` | Move uma peça para cima | `(c 6 4)` |
| | `b` | Move uma peça para baixo | `(b 2 3)` |
| **Capturas** | `cd` | Salta sobre peça adversária para a direita | `(cd 4 2)` |
| | `ce` | Salta sobre peça adversária para a esquerda | `(ce 4 6)` |
| | `cc` | Salta sobre peça adversária para cima | `(cc 6 4)` |
| | `cb` | Salta sobre peça adversária para baixo | `(cb 2 4)` |

## 6.2. Registos e Logs

O programa fornece feedback contínuo:

1. **Ecrã:** Exibe o tabuleiro atualizado a cada turno, indicando o jogador atual. Se for a vez do computador, mostra o tempo gasto e o número de nós analisados.
2. **Ficheiro `log.dat`:** Todas as jogadas são registadas neste ficheiro para análise posterior, incluindo:
    1. **Data e Hora.**
    2. **Jogada Efetuada.**
    3. **Nós Analisados.**
    4. **Cortes Alfa e Cortes Beta Efetuados.**
    5. **Tempo de Execução.**
