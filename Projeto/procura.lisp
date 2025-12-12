;;; procura.lisp
;;; Algoritmos de Procura e Estruturas de Dados
;;; Implementacao baseada nos Guioes de Laboratorio 8 e 9

;;; ---------------------------------------------------------
;;; 1. TIPO ABSTRATO DE DADOS: NO (Conforme Lab 8)
;;; Estrutura: (estado profundidade heuristica pai)
;;; ---------------------------------------------------------

(defun criar-no (estado &optional (g 0) (h 0) (pai nil))
  "Construtor do no: aceita estado, g, h e pai."
  (list estado g h pai))

(defun no-estado (no) (first no))
(defun no-profundidade (no) (second no)) ; g(n)
(defun no-g (no) (second no))            ; Alias
(defun no-heuristica (no) (third no))    ; h(n)
(defun no-pai (no) (fourth no))
(defun no-custo (no) (+ (no-g no) (no-heuristica no))) ; f(n)

;;; ---------------------------------------------------------
;;; 2. FUNCOES AUXILIARES E GERACAO
;;; ---------------------------------------------------------

(defun solucaop (no)
  "Verifica se um no e solução (apenas 1 pino restante)."
  (= (contar-pinos (no-estado no)) 1))

(defun ordenar-nos (lista-nos)
  "Ordena uma lista de nos por ordem crescente de custo f(n)."
  (sort lista-nos #'< :key #'no-custo))

(defun colocar-sucessores-em-abertos (abertos sucessores)
  "Junta sucessores a lista de abertos e ordena-a (para o A*)."
  (ordenar-nos (append abertos sucessores)))

(defun gerar-sucessores (no heuristica-fn)
  "Gera a lista de nos sucessores a partir de um no."
  (let ((estado (no-estado no)) (g (no-g no)))
    (gerar-sucessores-rec estado g heuristica-fn 1 1)))

(defun gerar-sucessores-rec (tab g h-fn l c)
  (cond ((> l 7) nil)
        ((> c 7) (gerar-sucessores-rec tab g h-fn (1+ l) 1))
        (t (append (tentar-ops l c tab g h-fn)
                   (gerar-sucessores-rec tab g h-fn l (1+ c))))))

(defun tentar-ops (l c tab g h-fn)
  (let ((ops (operadores)))
    (mapcan (lambda (op)
              (let ((nt (funcall op l c tab)))
                (if nt (list (criar-no nt (1+ g) 
                                       (if h-fn (funcall h-fn nt) 0) 
                                       nil)) nil)))
            ops)))

;;; ---------------------------------------------------------
;;; 3. METRICAS DE DESEMPENHO (Conforme Lab 9)
;;; ---------------------------------------------------------

(defun penetrancia (L T-total)
  (if (zerop T-total) 0 (float (/ L T-total))))

(defun ramificacao-media (L T-total)
  (if (<= T-total L) 1.0 (bisseccao L T-total 1.0 10.0 0.01)))

(defun bisseccao (L T-alvo min max erro)
  (let* ((b (/ (+ min max) 2.0)) (val (polinomio-b b L)))
    (cond ((< (abs (- val T-alvo)) erro) b)
          ((> val T-alvo) (bisseccao L T-alvo min b erro))
          (t (bisseccao L T-alvo b max erro)))))

(defun polinomio-b (b L)
  (if (= b 1) (1+ L) (/ (- (expt b (1+ L)) 1) (- b 1))))

;;; ---------------------------------------------------------
;;; 4. ALGORITMOS DE PROCURA (Retorno: no, gerados, expandidos, tempo)
;;; ---------------------------------------------------------

(defun bfs (no-inicial)
  (let ((inicio (get-internal-run-time)) (ger 1) (exp 0))
    (labels ((bfs-rec (abertos fechados)
               (if (null abertos) nil
                   (let ((atual (car abertos)))
                     (if (solucaop atual)
                         (list atual ger exp (- (get-internal-run-time) inicio))
                         (if (member (no-estado atual) fechados :test #'equal)
                             (bfs-rec (cdr abertos) fechados)
                             (progn (incf exp)
                               (let ((suc (gerar-sucessores atual nil)))
                                 (setf suc (mapcar (lambda (s) (criar-no (no-estado s) (no-g s) 0 atual)) suc))
                                 (incf ger (length suc))
                                 (bfs-rec (append (cdr abertos) suc) (cons (no-estado atual) fechados))))))))))
      (bfs-rec (list no-inicial) nil))))

(defun dfs (no-inicial prof-max)
  (let ((inicio (get-internal-run-time)) (ger 1) (exp 0))
    (labels ((dfs-rec (abertos fechados)
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