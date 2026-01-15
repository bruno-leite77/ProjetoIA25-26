;;; procura.lisp
;;; FASE 1: Algoritmos de Procura (BFS, DFS, A*) - OTIMIZADO

;;; --- TAD NO ---
(defun criar-no (estado &optional (g 0) (h 0) (pai nil)) (list estado g h pai))
(defun no-estado (no) (first no))
(defun no-g (no) (second no))
(defun no-heuristica (no) (third no))
(defun no-pai (no) (fourth no))
(defun no-custo (no) (+ (no-g no) (no-heuristica no)))

(defun solucaop (no) (= (contar-pinos (no-estado no)) 1))
(defun ordenar-nos (lista) (sort (copy-list lista) #'< :key #'no-custo))
(defun colocar-sucessores-em-abertos (abertos sucessores) (ordenar-nos (append abertos sucessores)))

;;; --- GERACAO SUCESSORES (Otimizada com MAPCAN) ---
(defun gerar-sucessores (no h-fn)
  (let ((estado (no-estado no)) (g (no-g no)))
    ;; OTIMIZACAO: mapcan evita recursao profunda na pilha
    (mapcan (lambda (l)
              (mapcan (lambda (c)
                        (tentar-ops l c estado g h-fn))
                      '(1 2 3 4 5 6 7)))
            '(1 2 3 4 5 6 7))))

(defun tentar-ops (l c tab g h-fn)
  ;; Chama os operadores da Fase 1 (definidos no puzzle.lisp)
  (let ((ops (operadores))) 
    (mapcan (lambda (op)
              (let ((nt (funcall op l c tab)))
                (if nt (list (criar-no nt (1+ g) (if h-fn (funcall h-fn nt) 0) nil)) nil)))
            ops)))

;;; --- METRICAS ---
(defun penetrancia (L T-total) (if (zerop T-total) 0 (float (/ L T-total))))
(defun polinomio-b (b L) (if (= b 1) (1+ L) (/ (- (expt b (1+ L)) 1) (- b 1))))
(defun bisseccao (L T-alvo min max erro)
  (let* ((b (/ (+ min max) 2.0)) (val (polinomio-b b L)))
    (cond ((< (abs (- val T-alvo)) erro) b)
          ((> val T-alvo) (bisseccao L T-alvo min b erro))
          (t (bisseccao L T-alvo b max erro)))))
(defun ramificacao-media (L T-total) (if (<= T-total L) 1.0 (bisseccao L T-total 1.0 10.0 0.01)))

;;; --- ALGORITMOS (Com declaracoes de otimizacao OBRIGATORIAS) ---

(defun bfs (no-inicial)
  (let ((inicio (get-internal-run-time)) (ger 1) (exp 0))
    (labels ((bfs-rec (abertos fechados)
               ;; OTIMIZACAO: Permite recursao de cauda sem estourar a pilha
               (declare (optimize (speed 3) (safety 0) (debug 0)))
               (if (null abertos) nil
                   (let ((atual (car abertos)))
                     (if (solucaop atual)
                         (list atual ger exp (- (get-internal-run-time) inicio))
                         (if (member (no-estado atual) fechados :test #'equal)
                             (bfs-rec (cdr abertos) fechados)
                             (progn 
                               (incf exp)
                               (let ((suc (gerar-sucessores atual nil)))
                                 (setf suc (mapcar (lambda (s) (criar-no (no-estado s) (no-g s) 0 atual)) suc))
                                 (incf ger (length suc))
                                 (bfs-rec (append (cdr abertos) suc) (cons (no-estado atual) fechados))))))))))
      (bfs-rec (list no-inicial) nil))))

(defun dfs (no-inicial prof-max)
  (let ((inicio (get-internal-run-time)) (ger 1) (exp 0))
    (labels ((dfs-rec (abertos fechados)
               (declare (optimize (speed 3) (safety 0) (debug 0)))
               (if (null abertos) nil
                   (let ((atual (car abertos)))
                     (cond ((solucaop atual) 
                            (list atual ger exp (- (get-internal-run-time) inicio)))
                           ((> (no-g atual) prof-max) (dfs-rec (cdr abertos) fechados))
                           ((member (no-estado atual) fechados :test #'equal) (dfs-rec (cdr abertos) fechados))
                           (t (incf exp)
                              (let ((suc (gerar-sucessores atual nil)))
                                (setf suc (mapcar (lambda (s) (criar-no (no-estado s) (no-g s) 0 atual)) suc))
                                (incf ger (length suc))
                                (dfs-rec (append suc (cdr abertos)) (cons (no-estado atual) fechados)))))))))
      (dfs-rec (list no-inicial) nil))))

(defun a-star (no-inicial h-fn)
  (let ((inicio (get-internal-run-time)) (ger 1) (exp 0))
    (labels ((a-star-rec (abertos fechados)
               (declare (optimize (speed 3) (safety 0) (debug 0)))
               (if (null abertos) nil
                   (let ((atual (car abertos)))
                     (if (solucaop atual) 
                         (list atual ger exp (- (get-internal-run-time) inicio))
                         (if (and (member (no-estado atual) fechados :test #'equal)
                                  (<= (no-custo (find (no-estado atual) fechados :key #'no-estado :test #'equal)) (no-custo atual)))
                             (a-star-rec (cdr abertos) fechados)
                             (progn (incf exp)
                               (let ((suc (gerar-sucessores atual h-fn)))
                                 (setf suc (mapcar (lambda (s) (criar-no (no-estado s) (no-g s) (no-heuristica s) atual)) suc))
                                 (incf ger (length suc))
                                 (a-star-rec (colocar-sucessores-em-abertos (cdr abertos) suc) (cons (no-estado atual) fechados))))))))))
      (a-star-rec (list no-inicial) nil))))