;;; algoritmo.lisp
;;; Algoritmo Negamax com cortes Alfa-Beta (Fase 2)
;;; Codigo desenvolvido com auxilio de IA (Gemini).

(defun negamax (estado prof alfa beta jogador f-suc f-aval t-limite inicio)
  "Retorna (valor melhor-jogada nos-analisados cortes-alfa cortes-beta)"
  (cond 
    ((or (zerop prof) (>= (- (get-internal-real-time) inicio) t-limite))
     (list (funcall f-aval estado jogador) nil 1 0 0))
    (t
     (let ((sucessores (funcall f-suc estado jogador)))
       (if (null sucessores)
           (list (funcall f-aval estado jogador) nil 1 0 0)
           (processar-negamax sucessores prof alfa beta jogador f-suc f-aval t-limite inicio -999999 nil 0 0 0))))))

(defun processar-negamax (sucs prof alfa beta jog f-suc f-aval t-limite ini melhor-v melhor-j total-nos c-alfa c-beta)
  (if (null sucs)
      (list melhor-v melhor-j total-nos c-alfa c-beta)
      (let* ((atual (car sucs))
             (res-filho (negamax (cdr atual) (1- prof) (- beta) (- alfa) (if (= jog 1) 2 1) f-suc f-aval t-limite ini))
             (v (- (first res-filho)))
             (nos-filho (third res-filho))
             (novo-total (+ total-nos nos-filho)))
        (cond 
          ((>= v beta) (list v (car atual) novo-total c-alfa (1+ c-beta)))
          ((> v melhor-v) 
           (processar-negamax (cdr sucs) prof (max alfa v) beta jog f-suc f-aval t-limite ini v (car atual) novo-total (if (> v alfa) (1+ c-alfa) c-alfa) c-beta))
          (t (processar-negamax (cdr sucs) prof alfa beta jog f-suc f-aval t-limite ini melhor-v melhor-j novo-total c-alfa c-beta))))))