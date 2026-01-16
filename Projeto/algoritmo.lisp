;;; algoritmo.lisp - Negamax com cortes Alfa-Beta
;;; Codigo gerado automaticamente conforme normas de programacao funcional.

(defun negamax (estado prof alfa beta jogador f-suc f-aval t-limite inicio)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond 
    ((or (zerop prof) (>= (- (get-internal-real-time) inicio) t-limite))
     (list (funcall f-aval estado jogador) nil 1 0 0))
    (t
     (let ((sucessores (funcall f-suc estado jogador)))
       (if (null sucessores)
           (list (funcall f-aval estado jogador) nil 1 0 0)
           (processar-negamax sucessores prof alfa beta jogador f-suc f-aval t-limite inicio -999999 nil 0 0 0))))))

(defun processar-negamax (sucs prof alfa beta jog f-suc f-aval t-lim ini melhor-v melhor-j t-nos c-alfa c-beta)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (null sucs)
      (list melhor-v melhor-j t-nos c-alfa c-beta)
      (let* ((atual (car sucs))
             (res-filho (negamax (cdr atual) (1- prof) (- beta) (- alfa) (adversario jog) f-suc f-aval t-lim ini))
             (v (- (first res-filho))))
        (if (> v melhor-v)
            (let ((novo-alfa (max alfa v)))
              (if (>= novo-alfa beta)
                  (list v (car atual) (+ t-nos (third res-filho)) c-alfa (1+ c-beta))
                  (processar-negamax (cdr sucs) prof novo-alfa beta jog f-suc f-aval t-lim ini v (car atual) (+ t-nos (third res-filho)) c-alfa c-beta)))
            (processar-negamax (cdr sucs) prof alfa beta jog f-suc f-aval t-lim ini melhor-v melhor-j (+ t-nos (third res-filho)) c-alfa c-beta)))))