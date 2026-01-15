;;; jogo.lisp
;;; Interface e Funcao de Campeonato (Fase 2)

(load "algoritmo.lisp")
(load "puzzle.lisp")

(defun jogar (estado tempo)
  "Funcao campeonato conforme Seccao 8 do enunciado."
  (let* ((inicio (get-internal-real-time))
         (j-atual (identificar-j estado))
         ;; Chama o algoritmo definido em algoritmo.lisp (negamax)
         (res (negamax estado 4 -999999 999999 j-atual #'gerar-todos-sucessores #'avaliar-estado tempo inicio)))
    (if (second res)
        (list (second res) (cdr (assoc (second res) (gerar-todos-sucessores estado j-atual) :test #'equal)))
        nil)))

(defun identificar-j (estado)
  (if (>= (contar-pecas-j estado 1) (contar-pecas-j estado 2)) 1 2))