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