;;; jogo.lisp - Interface e Regras de Jogo
;;; Codigo gerado automaticamente conforme normas de programacao funcional.

(load "algoritmo.lisp")
(load "puzzle.lisp")

(defun gerar-sucessores-f2 (tab j)
  "Gera sucessores com a regra da 1ª jogada."
  (let ((movs (mapcan (lambda (l) 
                        (mapcan (lambda (c) (tentar-mov-f2 l c tab j)) '(1 2 3 4 5 6 7))) 
                      '(1 2 3 4 5 6 7))))
    (if (jogo-inicial-p tab) (filtrar-primeira-jogada movs j) movs)))

(defun jogo-inicial-p (tab)
  (and (= (contar-pecas-j tab 1) 6) (= (contar-pecas-j tab 2) 6)))

(defun contar-pecas-j (tab j) (reduce #'+ (mapcar (lambda (lin) (count j lin)) tab)))

(defun filtrar-primeira-jogada (movs j)
  (remove-if-not (lambda (p) (let ((tipo (car (car p)))) (if (= j 1) (eq tipo 'b) (eq tipo 'c)))) movs))

(defun jogar (estado tempo)
  "Funcao principal exigida para o campeonato."
  (let* ((inicio (get-internal-real-time))
         (j-atual (if (>= (contar-pecas-j estado 1) (contar-pecas-j estado 2)) 1 2))
         (res (negamax estado 4 -999999 999999 j-atual #'gerar-sucessores-f2 #'avaliar-estado tempo inicio)))
    (list (second res) (cdr (assoc (second res) (gerar-sucessores-f2 estado j-atual) :test #'equal)))))