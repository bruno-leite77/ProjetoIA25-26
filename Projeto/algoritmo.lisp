;;; algoritmo.lisp
;;; Implementacao generica do Negamax com cortes Alfa-Beta

(defun negamax-alfa-beta (estado profundidade alfa beta jogador funcao-sucessores funcao-avaliacao tempo-limite inicio)
  "Retorna (valor melhor-jogada)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond 
    ((or (zerop profundidade) (>= (- (get-internal-real-time) inicio) tempo-limite))
     (list (funcall funcao-avaliacao estado jogador) nil))
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
             (res (negamax-alfa-beta (cdr suc) (1- prof) (- beta) (- alfa) 
                                    (if (= jog 1) 2 1) f-suc f-aval t-lim ini))
             (val (- (car res))))
        (if (> val melhor-v)
            (let ((novo-alfa (max alfa val)))
              (if (>= novo-alfa beta)
                  (list val (car suc)) ;; Corte Beta
                  (processar-sucessores (cdr lista) prof novo-alfa beta jog 
                                        f-suc f-aval t-lim ini val (car suc))))
            (processar-sucessores (cdr lista) prof alfa beta jog 
                                  f-suc f-aval t-lim ini melhor-v melhor-j)))))