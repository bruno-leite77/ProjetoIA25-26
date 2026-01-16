;;; jogo.lisp - Gestao de Partidas

(defun gerar-sucessores-f2 (tab j)
  (let ((movs (mapcan (lambda (l) (mapcan (lambda (c) (tentar-mov-f2 l c tab j)) '(1 2 3 4 5 6 7))) '(1 2 3 4 5 6 7))))
    (if (and (= (reduce #'+ (mapcar (lambda (lin) (count 1 lin)) tab)) 6)
             (= (reduce #'+ (mapcar (lambda (lin) (count 2 lin)) tab)) 6))
        (remove-if-not (lambda (p) (let ((tipo (car (car p)))) (if (= j 1) (eq tipo 'b) (eq tipo 'c)))) movs)
        movs)))

(defun ciclo-jogo (estado j-atual tipo-j1 tipo-j2 tempo)
  (imprimir-tabuleiro estado)
  (cond 
    ((vitoria-p estado 1) (format t "~%Vitoria do Jogador 1!~%"))
    ((vitoria-p estado 2) (format t "~%Vitoria do Jogador 2!~%"))
    (t (let ((tipo-atual (if (= j-atual 1) tipo-j1 tipo-j2)))
         (format t "~%Turno do Jogador ~d (~a): " j-atual tipo-atual)
         (let ((jogada (if (eq tipo-atual 'H) (ler-jogada-humana estado j-atual) (segundo (negamax-alfa-beta estado 4 -999999 999999 j-atual #'gerar-sucessores-f2 #'avaliar-estado tempo (get-internal-real-time))))))
           (if (null jogada)
               (format t "Sem jogadas possiveis. Passa turno.~%")
               (let ((novo-estado (cdr (assoc jogada (gerar-sucessores-f2 estado j-atual) :test #'equal))))
                 (format t "Jogada efetuada: ~a~%" jogada)
                 (ciclo-jogo novo-estado (if (= j-atual 1) 2 1) tipo-j1 tipo-j2 tempo))))))))

(defun ler-jogada-humana (tab j)
  (format t "~%Insira jogada (Tipo L C): ")
  (let ((entrada (read)))
    (if (assoc entrada (gerar-sucessores-f2 tab j) :test #'equal) entrada (progn (format t "Invalida!") (ler-jogada-humana tab j)))))