;;; jogo.lisp - Versao Otimizada (Sem Stack Overflow)
(load "algoritmo.lisp")
(load "puzzle.lisp")

;;; --- FUNCOES AUXILIARES ---

(defun gerar-sucessores-f2 (tab j)
  (let ((movs (mapcan (lambda (l) 
                        (mapcan (lambda (c) (tentar-mov-f2 l c tab j)) '(1 2 3 4 5 6 7))) 
                      '(1 2 3 4 5 6 7))))
    (if (jogo-inicial-p tab) (filtrar-primeira-jogada movs j) movs)))

(defun jogo-inicial-p (tab)
  "Verifica se o tabuleiro e EXATAMENTE o inicial."
  (equal tab '((nil nil 1 1 1 nil nil)
               (nil nil 1 1 1 nil nil)
               (0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0)
               (nil nil 2 2 2 nil nil)
               (nil nil 2 2 2 nil nil))))

(defun contar-pecas-j (tab j) (reduce #'+ (mapcar (lambda (lin) (count j lin)) tab)))

(defun filtrar-primeira-jogada (movs j)
  (remove-if-not (lambda (p) (let ((tipo (car (car p)))) (if (= j 1) (eq tipo 'b) (eq tipo 'c)))) movs))

(defun vitoria-p (tab j)
  (let ((objetivos (if (= j 1) 
                       '((6 3) (6 4) (6 5) (7 3) (7 4) (7 5))
                       '((1 3) (1 4) (1 5) (2 3) (2 4) (2 5)))))
    (some (lambda (coord) 
            (eql (celula (first coord) (second coord) tab) j)) 
          objetivos)))

(defun escrever-log-fase2 (jogada nos c-alfa c-beta tempo)
  (with-open-file (out "log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~%[Fase 2] Data: ~a | Jogada: ~a | Nos: ~d | Cortes A: ~d | Cortes B: ~d | Tempo: ~d ms" 
            (get-universal-time) jogada nos c-alfa c-beta tempo)))

;;; --- LEITURA DE JOGADA HUMANA ---

(defun ler-jogada-humana (tab j)
  (format t "~%[Jogador ~d] O seu turno." j)
  (if (jogo-inicial-p tab)
      (format t " (Nota: 1a jogada obrigatoria: ~a)" (if (= j 1) "Baixo (b)" "Cima (c)")))
  
  (format t "~%Introduza jogada com parentesis ex: (d 2 3) > ")
  (let ((entrada (read)))
    (let ((jogada-valida (assoc entrada (gerar-sucessores-f2 tab j) :test #'equal)))
      (if jogada-valida
          (car jogada-valida)
          (progn 
            (format t "~%Jogada Invalida! Tente novamente.") 
            (ler-jogada-humana tab j))))))

;;; --- CICLO DE JOGO (Com Otimizacao e Limite) ---

(defun ciclo-jogo (estado j-atual tipo-j1 tipo-j2 tempo &optional (num-jogada 1))
  ;; OTIMIZACAO CRITICA: Isto impede o Stack Overflow reciclando a memoria
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  
  (format t "~%---------------------------------------------------")
  (format t "~%Jogada Global: ~d" num-jogada)
  (imprimir-tabuleiro estado)
  
  (cond 
    ;; 1. Vitoria
    ((vitoria-p estado 1) (format t "~%!!! VITORIA JOGADOR 1 !!!~%"))
    ((vitoria-p estado 2) (format t "~%!!! VITORIA JOGADOR 2 !!!~%"))
    
    ;; 2. Empate por Limite de Jogadas (Evita loops infinitos)
    ((> num-jogada 150) 
     (format t "~%!!! EMPATE (Limite de 150 jogadas atingido) !!!~%"))
    
    (t 
     (let ((tipo-atual (if (= j-atual 1) tipo-j1 tipo-j2)))
       (format t "~%Turno: Jogador ~d (~a)" j-atual tipo-atual)
       
       (if (or (eq tipo-atual 'H) (eq tipo-atual 'humano) (eq tipo-atual 'h))
           
           ;; >>> HUMANO <<<
           (let ((jogada (ler-jogada-humana estado j-atual)))
             (let ((novo-estado (cdr (assoc jogada (gerar-sucessores-f2 estado j-atual) :test #'equal))))
               (ciclo-jogo novo-estado (adversario j-atual) tipo-j1 tipo-j2 tempo (1+ num-jogada))))
           
           ;; >>> COMPUTADOR <<<
           (progn
             (format t "~%O computador esta a pensar...~%")
             (let* ((inicio (get-internal-real-time))
                    (res (negamax-alfa-beta estado 4 -999999 999999 j-atual #'gerar-sucessores-f2 #'avaliar-estado tempo inicio))
                    (jogada (second res))
                    (nos (third res))
                    (c-alfa (fourth res))
                    (c-beta (fifth res))
                    (tempo-gasto (- (get-internal-real-time) inicio)))
               
               (if (null jogada)
                   (progn
                     (format t "~%O computador nao tem jogadas validas. Passa a vez.~%")
                     (ciclo-jogo estado (adversario j-atual) tipo-j1 tipo-j2 tempo (1+ num-jogada)))
                   (progn
                     (format t "~%Jogada PC: ~a (Tempo: ~d ms | Nos: ~d)" jogada tempo-gasto nos)
                     (escrever-log-fase2 jogada nos c-alfa c-beta tempo-gasto)
                     (let ((novo-estado (cdr (assoc jogada (gerar-sucessores-f2 estado j-atual) :test #'equal))))
                       (ciclo-jogo novo-estado (adversario j-atual) tipo-j1 tipo-j2 tempo (1+ num-jogada))))))))))))

;;; --- JOGAR (Compatibilidade) ---
(defun jogar (estado tempo)
  (let* ((inicio (get-internal-real-time))
         (j-atual (if (>= (contar-pecas-j estado 1) (contar-pecas-j estado 2)) 1 2))
         (res (negamax estado 4 -999999 999999 j-atual #'gerar-sucessores-f2 #'avaliar-estado tempo inicio)))
    (if (second res)
        (list (second res) (cdr (assoc (second res) (gerar-sucessores-f2 estado j-atual) :test #'equal)))
        nil)))