;;; 1. Domínio de Aplicação (solitaire.lisp)
;; 1.1. Seletores e Funções Auxiliares

(defun linha (l tabuleiro)
    "Retorna a linha (l) 1-indexada do tabuleiro."
    (nth (- l 1) tabuleiro)
)

(defun celula (l c tabuleiro)
    "Retorna o valor da célula (l, c) 1-indexada."
    (if (or (< l 1) (> l 7) (< c 1) (> c 7))
        nil ; Fora dos limites
        (nth (- c 1) (nth (- l 1) tabuleiro))
    )
)

(defun celula-validap (l c tabuleiro)
    "Verifica se a célula (l, c) é jogável (0 ou 1)."
    (let ((val (celula l c tabuleiro)))
        (if (or (= val 0) (= val 1))
            t
            nil
        )
    )
)

;; 1.2. Modificação de Estado

(defun substituir-posicao (idx lista valor)
    "Retorna nova lista com a posição (idx) 1-indexada substituída."
    (if (null lista)
        nil
        (if (= idx 1)
            (cons valor (cdr lista))
            (cons (car lista) (substituir-posicao (- idx 1) (cdr lista) valor))
        )
    )
)

(defun substituir (l c tabuleiro valor)
    "Retorna novo tabuleiro com a célula (l, c) 1-indexada substituída."
    (if (null tabuleiro)
        nil
        (if (= l 1)
            (cons (substituir-posicao c (car tabuleiro) valor) (cdr tabuleiro))
            (cons (car tabuleiro) (substituir (- l 1) c (cdr tabuleiro) valor))
        )
    )
)

;; 1.3. Operadores

(defun operador-cd (l c tab)
    "Aplicar 'Captura Direita' na peça (l, c). Retorna o novo estado ou NIL se inválido."
    
    ;; Validar limites antes de aceder à célula
    (if (> (+ c 2) 7) (return-from operador-cd nil))

    (let ((origem (celula l c tab))
          (meio (celula l (+ c 1) tab))
          (destino (celula l (+ c 2) tab)))

        ;; Validar condições da captura
        (if (and (= origem 1) (= meio 1) (= destino 0))
            ;; Aplicar movimento
            (let* ((tab1 (substituir l c tab 0))         ; Esvazia origens
                   (tab2 (substituir l (+ c 1) tab1 0))  ; Remove pino meio
                   (tab3 (substituir l (+ c 2) tab2 1))) ; Move pino
                tab3)
            ;; Movimento inválido
            nil
        )

    )
)

(defun operador-ce (l c tab)
    "Aplica 'Captura Esquerda' na peça (l, c). Retorna o novo estado ou NIL se inválido"
    (if (< (- c 2) 1) (return-from operador-ce nil))

    (let ((origem (celula l c tab))
          (meio (celula l (- c 1) tab))
          (destino (celula l (- c 2) tab)))

        (if (and (= origem 1) (= meio 1) (= destino 0))
            (let* ((tab1 (substituir l c tab 0))
                   (tab2 (substituir l (- c 1) tab1 0))
                   (tab3 (substituir l (- c 2) tab2 1)))
                tab3)
            nil
        )
    )
)

(defun operador-cc (l c tab)
    "Aplica 'Captura Cima' na peça (l, c). Retorna o novo estado ou NIL se inválido"
    (if (< (- l 2) 1) (return-from operador-cc nil))

    (let ((origem (celula l c tab))
          (meio (celula (- l 1) c tab))
          (destino (celula (- l 2) c tab)))

        (if (and (= origem 1) (= meio 1) (= destino 0))
            (let* ((tab1 (substituir l c tab 0))
                   (tab2 (substituir (- l 1) c tab1 0))
                   (tab3 (substituir (- l 2) c tab2 1)))
                tab3)
            nil
        )
    )
)

(defun operador-cb (l c tab)
    "Aplica 'Captura Baixo' na peça (l, c). Retorna o novo estado ou NIL se inválido"
    (if (> (+ l 2) 7) (return-from operador-cb nil))

    (let ((origem (celula l c tab))
          (meio (celula (+ l 1) c tab))
          (destino (celula (+ l 2) c tab)))

        (if (and (= origem 1) (= meio 1) (= destino 0))
            (let* ((tab1 (substituir l c tab 0))
                   (tab2 (substituir (+ l 1) c tab1 0))
                   (tab3 (substituir (+ l 2) c tab2 1)))
                tab3)
            nil
        )
    )
)

;; 1.4. Heurísticas Específicas do Domínio

(defun contar-pinos (tabuleiro)
    "Função auxiliar para contar pinos (1s) no estado."
    (if (null tabuleiro)
        0
        (+ (count 1 (car tabuleiro))
           (contar-pinos (cdr tabuleiro)))
    )
)

(defun h1-solitario (estado)
    "Heurística 1 (Admissível): (Número de pinos) - 1."
    (max 0 (- (contar-pinos estado) 1))
)

(defun h-zero (estado)
    "Heurística nula para BFS e DFS."
    (declare (ignore estado)) ; Ignora o estado, retorna sempre 0
    0
)

;;; 2. Procura Genérica (search.lisp)
;; 2.1. Estrutura do Nó

(defun no-estado (no)
    "Retorna o estado do nó."
    (first no)
)

(defun no-profundidade (no)
    "Retorna a profundidade do nó."
    (second no)
)

(defun no-heuristica (no)
    "Retorna o valor heurístico h(n) do nó."
    (third no)
)

(defun no-pai (no)
    "Retorna o nó pai."
    (fourth no)
)

(defun no-custo (no)
    "Retorna o custo total f(n) = g(n) + h(n)."
    (+ (no-profundidade no) (no-heuristica no))
)

;; 2.2. Funções Auxiliares

(defun no-solucaop (no)
    "Predicado: Verifica se o nó é solução (1 pino)."
    (= (contar-pinos (no-estado no )) 1)
)

(defun no-existep (no lista-nos)
    "Predicado: Verifica se o ESTADO do nó existe na lista."
    (if (null lista-nos)
        nil
        (if (equal (no-estado no) (no-estado (car lista-nos)))
            t
            (no-existep no (cdr lista-nos))
        )
    )
)

(defun no-existep-custo-menor (no lista-nos)
    "Verifica se um nó com o mesmo estado e custo g(n) menor ou igual existe na lista."
    (if (null lista-nos)
        nil
        (let ((no-existente (car lista-nos)))
            (if (and (equal (no-estado no) (no-estado no-existente))
                     (<= (no-profundidade no-existente) (no-profundidade no)))
                t ; Encontrado um nó com estado igual e custo menor ou igual
                (no-existep-custo-menor no (cdr lista-nos))
            )
        )
    )
)

;; 2.3. Gestão de Listas

(defun abertos-bfs (abertos sucessores)
    "FIFO: Adiciona sucessores ao FIM da lista."
    (append abertos sucessores)
)

(defun aberto-dfs (abertos sucessores)
    "LIFO: Adiciona sucessores ao INÍCIO da lista."
    (append sucessores abertos)
)

(defun ordenar-nos (lista-nos)
    "Ordena a lista de nós por custo f(n) crescente."
    (sort (copy-list lista-nos) #'< :key #'no-custo)
)

(defun colocar-sucessores-em-abertos (abertos sucessores)
    "Junta as listas ABERTOS e SUCESSORES, e reordena-as."
    (ordenar-nos (append sucessores abertos))
)

;; 2.4. Geração de Sucessores

(defun sucessores (no funcao-heuristica)
    "Gera a lista de todos os nós sucessores válidos para o Solitário."
    (let ((estado-atual (no-estado no))
          (profundidade-atual (no-profundidade no))
          (lista-sucessores nil))

        ;; Iteração sobre o tabuleiro 7x7
        (loop for l from 1 to 7 do
            (loop for c from 1 to 7 do

                ;; Tentar os 4 operadores
                (dolist (operador-fn (list #'operador-cd #'operador-ce 
                                           #'operador-cc #'operador-cb))

                    (let ((novo-estado (funcall operador-fn l c estado-atual)))
                        
                        (when novo-estado
                            ;; Movimento válido. Criar nó com 4 elementos.
                            (let* ((g-novo (+ 1 g-atual))
                                   (h-novo (funcall funcao-heuristica novo-estado))
                                   (no-novo (list novo-estado g-novo h-novo no)))
                                (push no-novo lista-sucessores)
                            )
                        )
                    )
                )
            )
        (reverse lista-sucessores) ; (push inverte a ordem, 'reverse' corrige)
        )
    )
)

;; 2.5. Algoritmos de Procura

(defun bfs (no-inicial &optional (abertos (list no-inicial)) (fechados nil))
    "Implementa a procura Breath-First Search (BFS)."
    (if (null abertos)
        nil ; Falha: Abertos está vazia

        (let ((no-atual (car abertos)))
            (if (no-solucaop no-atual)
                no-atual ; Sucesso: Nó atual é solução

                ;; Evitar clicos
                (if (no-existep no-atual)
                    ;; Nó repetido, ignorar e continuar
                    (bfs no-inicial (cdr abertos) fechados)

                    ;; Nó novo, expandir
                    (let* ((novos-sucessores (sucessores no-atual))
                           (fechados-atualizados (cons no-atual fechados)))

                        (bfs no-inicial
                             (abertos-bfs (cdr abertos) novos-sucessores) ; FIFO
                             fechados-atualizados
                        )
                    )
                )
            )
        )
    )
)

(defun dfs (no-inicial profundidade-max &optional (abertos (list no-inicial)) (fechados nil))
    "Implementa a procura Depth-First Search (DFS)."
    (if (null abertos)
        nil ; Falha: Abertos está vazia

        (let ((no-atual (car abertos)))
            (if (no-solucaop no-atual)
                no-atual ; Sucesso

                (if (or (no-existep no-atual fechados)
                    (>= (no-profundidade no-atual) profundidade-max)) ; Limite de profundidade

                    ;; Nó repetido ou limite atingido, ignorar
                    (dfs no-inicial profundidade-max (cdr abertos) fechados)

                    ;; Nó novo, expandir
                    (let* ((novos-sucessores (sucessores no-atual))
                           (fechados-atualizados (cons no-atual fechados)))

                        (dfs no-inicial
                            profundidade-max
                            (aberto-fds (cdr abertos) novos-sucessores) ; LIFO
                            fechados-atualizados
                        )
                    )
                )
            )
        )
    )
)

(defun a* (no-inicial funcao-heristica &optional (abertos (list no-inicial)) (fechados nil))
    
)
