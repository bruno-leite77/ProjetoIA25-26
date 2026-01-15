;;; jogo.lisp
;;; Interface e Funcao de Campeonato (Fase 2)
;;; Codigo desenvolvido com auxilio de IA (Gemini).

;; CAMINHO CORRIGIDO: Usa / para evitar erros no Windows/LispWorks
(hcl:change-directory "C:/Users/guerr/OneDrive/Desktop/Faculdade/IA/ProjetoIA25-26/Projeto/")

(load "algoritmo.lisp")
(load "puzzle.lisp")

(defun jogar (estado tempo)
  "Funcao campeonato conforme Seccao 8 do enunciado."
  (let* ((inicio (get-internal-real-time))
         (j-atual (identificar-j estado))
         (res (negamax estado 4 -999999 999999 j-atual #'gerar-todos-sucessores #'avaliar-estado tempo inicio)))
    (registar-log res)
    (list (second res) (cdr (assoc (second res) (gerar-todos-sucessores estado j-atual) :test #'equal)))))

(defun identificar-j (estado)
  (if (>= (contar-pecas estado 1) (contar-pecas estado 2)) 1 2))

(defun registar-log (res)
  (with-open-file (f "log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
    (let ((msg (format nil "~%Jogada: ~A | Nos: ~D | Cortes A: ~D | Cortes B: ~D" 
                       (second res) (third res) (fourth res) (fifth res))))
      (format t "~A" msg)
      (write-string msg f))))