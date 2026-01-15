;;; puzzle.lisp
;;; Dominio do jogo Solitario 2 (Fase 2)
;;; Codigo desenvolvido com auxilio de IA (Gemini).

(defun celula (l c tab)
  (if (or (< l 1) (> l 7) (< c 1) (> c 7)) nil (nth (1- c) (nth (1- l) tab))))

(defun substituir (l c tab v)
  (cond ((null tab) nil)
        ((= l 1) (cons (substituir-posicao c (car tab) v) (cdr tab)))
        (t (cons (car tab) (substituir (1- l) c (cdr tab) v)))))

(defun substituir-posicao (i lista v)
  (cond ((null lista) nil) ((= i 1) (cons v (cdr lista))) (t (cons (car lista) (substituir-posicao (1- i) (cdr lista) v)))))

(defun gerar-todos-sucessores (tab j)
  (gerar-sucessores-rec tab j 1 1))

(defun gerar-sucessores-rec (tab j l c)
  "Corrigido: Aceita exatamente TAB J L C para evitar erro de definicao."
  (cond ((> l 7) nil)
        ((> c 7) (gerar-sucessores-rec tab j (1+ l) 1))
        (t (append (tentar-movimentos l c tab j) (gerar-sucessores-rec tab j l (1+ c))))))

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

(defun avaliar-estado (tab j)
  "Garante retorno numerico para evitar erro (+ NIL NIL)."
  (let ((eu (or (contar-pecas tab j) 0)) 
        (ele (or (contar-pecas tab (if (= j 1) 2 1)) 0)))
    (+ (* 100 (- eu ele)) (or (bonus-alvo tab j) 0))))

(defun contar-pecas (tab j) (reduce #'+ (mapcar (lambda (lin) (count j lin)) tab)))

(defun bonus-alvo (tab j)
  (let ((alvos (if (= j 1) '((6 3) (6 4) (6 5) (7 3) (7 4) (7 5)) '((1 3) (1 4) (1 5) (2 3) (2 4) (2 5)))))
    (if (some (lambda (a) (eql (celula (first a) (second a) tab) j)) alvos) 5000 0)))