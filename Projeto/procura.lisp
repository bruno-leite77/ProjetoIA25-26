;;; procura.lisp
;;; Modulo Generico de Procura (BFS, DFS, A*)

;;; 1. Estrutura do No
;;; Um no e uma lista: (estado g h pai)

(defun criar-no (estado &optional (g 0) (h 0) (pai nil))
  (list estado g h pai))

(defun no-estado (no) (first no))
(defun no-g (no) (second no))
(defun no-h (no) (third no))
(defun no-pai (no) (fourth no))
(defun no-f (no) (+ (no-g no) (no-h no)))

(defun solucaop (no)
  "Verifica se e solucao (apenas 1 pino restante)."
  (= (contar-pinos (no-estado no)) 1))

;;; 2. Geracao de Sucessores (Recursiva, sem loops)

(defun gerar-sucessores (no heuristica-fn)
  "Gera lista de nos sucessores aplicando todos os operadores validos."
  (gerar-sucessores-rec (no-estado no) (no-g no) heuristica-fn 1 1))

(defun gerar-sucessores-rec (tabuleiro g-pai heuristica-fn l c)
  "Percorre o tabuleiro recursivamente (l=linha, c=coluna) tentando movimentos."
  (cond
    ((> l 7) nil) ;; Terminou o tabuleiro
    ((> c 7) (gerar-sucessores-rec tabuleiro g-pai heuristica-fn (1+ l) 1)) ;; Muda de linha
    (t
     ;; Tenta os 4 operadores nesta posicao e concatena com o resto
     (append
      (tentar-operadores l c tabuleiro g-pai heuristica-fn)
      (gerar-sucessores-rec tabuleiro g-pai heuristica-fn l (1+ c))))))

(defun tentar-operadores (l c tab g-pai h-fn)
  "Tenta aplicar CD, CE, CC, CB na posicao (l,c)."
  (let ((ops (list #'operador-cd #'operador-ce #'operador-cc #'operador-cb)))
    (mapcan (lambda (op)
              (let ((novo-tab (funcall op l c tab)))
                (if novo-tab
                    (let ((h (if h-fn (funcall h-fn novo-tab) 0)))
                      (list (criar-no novo-tab (1+ g-pai) h nil))) 
                    nil)))
            ops)))

;;; 3. Algoritmos de Procura

;; BFS (Largura) - FIFO
(defun bfs (no-inicial)
  (bfs-rec (list no-inicial) nil))

(defun bfs-rec (abertos fechados)
  (if (null abertos) nil
      (let ((atual (car abertos)))
        (if (solucaop atual) atual
            (if (member (no-estado atual) fechados :test #'equal)
                (bfs-rec (cdr abertos) fechados) ;; Ignora repetidos
                (let ((sucessores (gerar-sucessores atual nil)))
                  (setf sucessores (mapcar (lambda (s) (criar-no (no-estado s) (no-g s) 0 atual)) sucessores))
                  (bfs-rec (append (cdr abertos) sucessores) 
                           (cons (no-estado atual) fechados))))))))

;; DFS (Profundidade) - LIFO
(defun dfs (no-inicial prof-max)
  (dfs-rec (list no-inicial) nil prof-max))

(defun dfs-rec (abertos fechados prof-max)
  (if (null abertos) nil
      (let ((atual (car abertos)))
        (cond
          ((solucaop atual) atual)
          ((> (no-g atual) prof-max) (dfs-rec (cdr abertos) fechados prof-max)) ;; Corte de profundidade
          ((member (no-estado atual) fechados :test #'equal) (dfs-rec (cdr abertos) fechados prof-max))
          (t 
           (let ((sucessores (gerar-sucessores atual nil)))
             (setf sucessores (mapcar (lambda (s) (criar-no (no-estado s) (no-g s) 0 atual)) sucessores))
             (dfs-rec (append sucessores (cdr abertos)) 
                      (cons (no-estado atual) fechados)
                      prof-max)))))))

;; A* (A-Star) - Ordenado por F
(defun a-star (no-inicial heuristica-fn)
  (a-star-rec (list no-inicial) nil heuristica-fn))

(defun a-star-rec (abertos fechados h-fn)
  (if (null abertos) nil
      (let ((atual (car abertos))) ;; Abertos esta ordenado por F
        (if (solucaop atual) atual
            (if (member (no-estado atual) fechados :test #'equal)
                (a-star-rec (cdr abertos) fechados h-fn)
                (let ((sucessores (gerar-sucessores atual h-fn)))
                  ;; Associa pai e reordena
                  (setf sucessores (mapcar (lambda (s) (criar-no (no-estado s) (no-g s) (no-h s) atual)) sucessores))
                  (let ((novos-abertos (append (cdr abertos) sucessores)))
                    ;; Ordena por F = G + H
                    (setf novos-abertos (sort novos-abertos #'< :key #'no-f))
                    (a-star-rec novos-abertos (cons (no-estado atual) fechados) h-fn))))))))