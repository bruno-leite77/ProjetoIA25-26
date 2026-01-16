;;; procura.lisp - Algoritmos de Procura e Metricas (Versao Final)
;;; Codigo gerado automaticamente conforme normas de programacao funcional.

;;; --- 1. ESTRUTURA DO NO ---
(defun criar-no (estado &optional (g 0) (h 0) (pai nil)) (list estado g h pai))
(defun no-estado (no) (first no))
(defun no-g (no) (second no))
(defun no-heuristica (no) (third no))
(defun no-pai (no) (fourth no))
(defun no-custo (no) (+ (no-g no) (no-heuristica no)))

;;; --- 2. AUXILIARES ---
(defun solucaop (no) (= (contar-pinos (no-estado no)) 1))

(defun ordenar-nos (lista) (sort (copy-list lista) #'< :key #'no-custo))

;; Funcao de sucessores generica para Fase 1
(defun gerar-sucessores-f1 (no h-fn)
  (let ((estado (no-estado no)) (g (no-g no)))
    (mapcan (lambda (l)
              (mapcan (lambda (c)
                        (mapcan (lambda (op)
                                  (let ((res (funcall op l c estado)))
                                    (if res (list (criar-no res (1+ g) (if h-fn (funcall h-fn res) 0) no)) nil)))
                                (operadores-f1)))
                      '(1 2 3 4 5 6 7)))
            '(1 2 3 4 5 6 7))))

;;; --- 3. METRICAS (ESTAVAM EM FALTA) ---
(defun penetrancia (L T-total)
  "Calcula a penetrancia: Comprimento solucao / Total nos gerados"
  (if (zerop T-total) 0 (float (/ L T-total))))

(defun ramificacao-media (L T-total)
  "Calcula o fator de ramificacao medio (B*) usando bisseccao."
  (if (<= T-total L) 1.0 (bisseccao L T-total 1.0 10.0 0.01)))

(defun bisseccao (L T-alvo min max erro)
  (let* ((b (/ (+ min max) 2.0)) (val (polinomio-b b L)))
    (cond ((< (abs (- val T-alvo)) erro) b)
          ((> val T-alvo) (bisseccao L T-alvo min b erro))
          (t (bisseccao L T-alvo b max erro)))))

(defun polinomio-b (b L)
  (if (= b 1) (1+ L) (/ (- (expt b (1+ L)) 1) (- b 1))))

;;; --- 4. ALGORITMOS DE PROCURA ---

;;; BFS (Largura)
(defun bfs (no-inicial)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((inicio (get-internal-run-time)))
    (labels ((bfs-rec (abertos fechados ger exp)
               (if (null abertos) nil
                   (let ((atual (car abertos)))
                     (if (solucaop atual)
                         (list atual ger exp (- (get-internal-run-time) inicio))
                         (if (member (no-estado atual) fechados :test #'equal)
                             (bfs-rec (cdr abertos) fechados ger exp)
                             (let ((suc (gerar-sucessores-f1 atual nil)))
                               (bfs-rec (append (cdr abertos) suc) (cons (no-estado atual) fechados) (+ ger (length suc)) (1+ exp)))))))))
      (bfs-rec (list no-inicial) nil 1 0))))

;;; DFS (Profundidade) - ADICIONADO
(defun dfs (no-inicial prof-max)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((inicio (get-internal-run-time)))
    (labels ((dfs-rec (abertos fechados ger exp)
               (if (null abertos) nil
                   (let ((atual (car abertos)))
                     (cond 
                       ((solucaop atual)
                        (list atual ger exp (- (get-internal-run-time) inicio)))
                       ((> (no-g atual) prof-max) 
                        (dfs-rec (cdr abertos) fechados ger exp))
                       ((member (no-estado atual) fechados :test #'equal)
                        (dfs-rec (cdr abertos) fechados ger exp))
                       (t
                        (let ((suc (gerar-sucessores-f1 atual nil)))
                          (dfs-rec (append suc (cdr abertos)) (cons (no-estado atual) fechados) (+ ger (length suc)) (1+ exp)))))))))
      (dfs-rec (list no-inicial) nil 1 0))))

;;; A* (Melhor Primeiro)
(defun a-star (no-inicial h-fn)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((inicio (get-internal-run-time)))
    (labels ((as-rec (abertos fechados ger exp)
               (if (null abertos) nil
                   (let ((atual (car abertos)))
                     (if (solucaop atual)
                         (list atual ger exp (- (get-internal-run-time) inicio))
                         (if (member (no-estado atual) fechados :test #'equal)
                             (as-rec (cdr abertos) fechados ger exp)
                             (let ((suc (gerar-sucessores-f1 atual h-fn)))
                               (as-rec (ordenar-nos (append (cdr abertos) suc)) 
                                       (cons (no-estado atual) fechados) 
                                       (+ ger (length suc)) 
                                       (1+ exp)))))))))
      (as-rec (list no-inicial) nil 1 0))))