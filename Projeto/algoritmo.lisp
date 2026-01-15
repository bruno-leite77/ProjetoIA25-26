;;; algoritmo.lisp
;;; FASE 2: Negamax com Alfa-Beta (Otimizado)

(defun negamax-alfa-beta (estado profundidade alfa beta jogador funcao-sucessores funcao-avaliacao tempo-limite inicio)
  ;; OTIMIZACAO: Forca Tail Call Optimization e reduz uso de stack
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  
  (cond 
    ((>= (- (get-internal-real-time) inicio) tempo-limite)
     (list (funcall funcao-avaliacao estado jogador) nil))
    
    ((zerop profundidade)
     (list (* (funcall funcao-avaliacao estado jogador) 1) nil)) 
    
    (t
     (let ((sucessores (funcall funcao-sucessores estado jogador)))
       (if (null sucessores)
           (list (funcall funcao-avaliacao estado jogador) nil)
           (processar-sucessores sucessores profundidade alfa beta jogador 
                                 funcao-sucessores funcao-avaliacao 
                                 tempo-limite inicio -9999999 nil))))))

(defun processar-sucessores (lista prof alfa beta jog f-suc f-aval t-lim ini melhor-v melhor-j)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  
  (if (null lista)
      (list melhor-v melhor-j)
      (let* ((suc (car lista))
             (jogada (car suc))
             (novo-est (cdr suc))
             (res (negamax-alfa-beta novo-est (1- prof) (- beta) (- alfa) 
                                    (adversario jog) f-suc f-aval t-lim ini))
             (val (- (first res))))
        
        (if (> val melhor-v)
            (let ((novo-alfa (max alfa val)))
              (if (>= novo-alfa beta)
                  (list val jogada) 
                  (processar-sucessores (cdr lista) prof novo-alfa beta jog 
                                        f-suc f-aval t-lim ini val jogada)))
            (processar-sucessores (cdr lista) prof alfa beta jog 
                                  f-suc f-aval t-lim ini melhor-v melhor-j)))))