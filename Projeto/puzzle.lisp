;;; puzzle.lisp
;;; Dominio do jogo Solitario 2 (Fase 2) - OTIMIZADO

;;; SELETORES E AUXILIARES
(defun linha (l tabuleiro) (nth (1- l) tabuleiro))
(defun celula (l c tab)
  (if (or (< l 1) (> l 7) (< c 1) (> c 7)) nil (nth (1- c) (nth (1- l) tab))))

(defun substituir-posicao (i lista v)
  (cond ((null lista) nil) ((= i 1) (cons v (cdr lista))) (t (cons (car lista) (substituir-posicao (1- i) (cdr lista) v)))))

(defun substituir (l c tab v)
  (cond ((null tab) nil)
        ((= l 1) (cons (substituir-posicao c (car tab) v) (cdr tab)))
        (t (cons (car tab) (substituir (1- l) c (cdr tab) v)))))

(defun adversario (jogador) (if (= jogador 1) 2 1))

;;; OPERADORES FASE 1 (Legado)
(defun aplicar-movimento (l c tab dl dc)
  (let* ((ld (+ l dl)) (cd (+ c dc)) (lm (+ l (/ dl 2))) (cm (+ c (/ dc 2))))
    (substituir ld cd (substituir lm cm (substituir l c tab 0) 0) 1)))

(defun operador-cd (l c tab) (if (<= (+ c 2) 7) (let ((o (celula l c tab)) (m (celula l (1+ c) tab)) (d (celula l (+ c 2) tab))) (if (and (eql o 1) (eql m 1) (eql d 0)) (aplicar-movimento l c tab 0 2) nil)) nil))
(defun operador-ce (l c tab) (if (>= (- c 2) 1) (let ((o (celula l c tab)) (m (celula l (1- c) tab)) (d (celula l (- c 2) tab))) (if (and (eql o 1) (eql m 1) (eql d 0)) (aplicar-movimento l c tab 0 -2) nil)) nil))
(defun operador-cc (l c tab) (if (>= (- l 2) 1) (let ((o (celula l c tab)) (m (celula (1- l) c tab)) (d (celula (- l 2) c tab))) (if (and (eql o 1) (eql m 1) (eql d 0)) (aplicar-movimento l c tab -2 0) nil)) nil))
(defun operador-cb (l c tab) (if (<= (+ l 2) 7) (let ((o (celula l c tab)) (m (celula (1+ l) c tab)) (d (celula (+ l 2) c tab))) (if (and (eql o 1) (eql m 1) (eql d 0)) (aplicar-movimento l c tab 2 0) nil)) nil))
(defun operadores () (list #'operador-cd #'operador-ce #'operador-cc #'operador-cb))

;; HEURISTICAS FASE 1
(defun contar-pinos (tab) (if (null tab) 0 (+ (count 1 (car tab)) (contar-pinos (cdr tab)))))
(defun h1-base (est) (/ 1.0 (1+ (contar-pinos est))))
(defun h2-extra (est) (1- (contar-pinos est)))

;;; FASE 2: GERAÇÃO DE SUCESSORES (OTIMIZADA COM MAPCAN)
(defun gerar-todos-sucessores (tab j)
  "Gera sucessores usando mapcan para evitar recursao de pilha."
  (mapcan (lambda (l)
            (mapcan (lambda (c)
                      (tentar-movimentos l c tab j))
                    '(1 2 3 4 5 6 7)))
          '(1 2 3 4 5 6 7)))

(defun tentar-movimentos (l c tab j)
  (let ((ops '((d 0 1 nil) (e 0 -1 nil) (c -1 0 nil) (b 1 0 nil)
               (cd 0 2 t) (ce 0 -2 t) (cc -2 0 t) (cb 2 0 t))))
    (mapcan (lambda (o)
              (let ((res (validar-e-aplicar l c tab j (second o) (third o) (fourth o))))
                (if res (list (cons (list (car o) l c) res)) nil)))
            ops)))

(defun validar-e-aplicar (l c tab j dl dc cap)
  (let ((nl (+ l dl)) (nc (+ c dc)) (adv (if (= j 1) 2 1)))
    (if (and (eql (celula l c tab) j) (eql (celula nl nc tab) 0))
        (if cap
            (let ((ml (+ l (/ dl 2))) (mc (+ c (/ dc 2))))
              (if (eql (celula ml mc tab) adv)
                  (substituir nl nc (substituir ml mc (substituir l c tab 0) 0) j) nil))
            (substituir nl nc (substituir l c tab 0) j))
        nil)))

;; AVALIACAO FASE 2
(defun avaliar-estado (tab j)
  (let ((eu (or (contar-pecas-j tab j) 0)) 
        (ele (or (contar-pecas-j tab (if (= j 1) 2 1)) 0)))
    (+ (* 100 (- eu ele)) (or (bonus-alvo tab j) 0))))

(defun contar-pecas-j (tab j) (reduce #'+ (mapcar (lambda (lin) (count j lin)) tab)))

(defun bonus-alvo (tab j)
  (let ((alvos (if (= j 1) '((6 3) (6 4) (6 5) (7 3) (7 4) (7 5)) '((1 3) (1 4) (1 5) (2 3) (2 4) (2 5)))))
    (if (some (lambda (a) (eql (celula (first a) (second a) tab) j)) alvos) 5000 0)))