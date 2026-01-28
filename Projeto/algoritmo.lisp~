;;; algoritmo.lisp
;;; Negamax com Cortes Alfa-Beta e Contagem de Nós - CORRIGIDO

(defun negamax-alfa-beta (estado profundidade alfa beta jogador funcao-sucessores funcao-avaliacao tempo-limite inicio)
  "Retorna uma lista: (valor melhor-jogada nos-analisados cortes-alfa cortes-beta)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; Chamada inicial do processo recursivo
  (processar-negamax (list (list nil estado)) ;; Lista ficticia para iniciar
                     (1+ profundidade) alfa beta jogador 
                     funcao-sucessores funcao-avaliacao 
                     tempo-limite inicio -9999999 nil 0 0 0 t))

(defun processar-negamax (lista prof alfa beta jog f-suc f-aval t-lim ini melhor-v melhor-j nos c-alfa c-beta raiz-p)
  "Funcao auxiliar que gere a recursividade e contagem."
  
  ;; 1. Verificar Tempo ou Profundidade Zero (apenas se nao for a raiz)
  (cond
    ((and (not raiz-p) (or (zerop prof) (>= (- (get-internal-real-time) ini) t-lim)))
     ;; Retorna valor heuristico, sem jogada, conta 1 no analisado
     (list (funcall f-aval (cdar lista) jog) nil 1 c-alfa c-beta))

    (t
     ;; Se for a raiz, geramos sucessores do estado inicial.
     ;; Se nao, usamos a lista de sucessores passada.
     (let ((sucessores (if raiz-p 
                           (funcall f-suc (second (car lista)) jog) 
                           lista)))
       
       (if (null sucessores)
           ;; Sem sucessores = Estado Terminal (ou folha)
           (list (funcall f-aval (if raiz-p (second (car lista)) (cdar lista)) jog) nil 1 c-alfa c-beta)
           
           ;; Processar lista de sucessores
           (iterar-sucessores sucessores (if raiz-p prof (1- prof)) alfa beta jog f-suc f-aval t-lim ini -9999999 nil nos c-alfa c-beta))))))

(defun iterar-sucessores (lista prof alfa beta jog f-suc f-aval t-lim ini melhor-v melhor-j nos c-alfa c-beta)
  "Itera sobre os irmaos (sucessores) aplicando Alfa-Beta."
  (if (null lista)
      (list melhor-v melhor-j nos c-alfa c-beta)
      
      (let* ((suc-atual (car lista))  ;; (jogada . estado)
             (jogada (car suc-atual))
             (estado (cdr suc-atual))
             
             ;; Chamada Recursiva (troca de jogador e inverte alfa/beta)
             (res (processar-negamax (list suc-atual) (1- prof) (- beta) (- alfa) 
                                     (if (= jog 1) 2 1) f-suc f-aval t-lim ini 0 nil 0 0 0 nil))
             
             (val (- (first res)))      ;; Valor invertido
             (nos-filho (third res))    ;; Nos analisados pelo filho
             (novos-nos (+ nos nos-filho))) ;; Acumula nos
        
        ;; Logica de Maximizacao
        (if (> val melhor-v)
            (let ((novo-alfa (max alfa val)))
              (if (>= novo-alfa beta)
                  ;; Corte Beta
                  (list val jogada novos-nos c-alfa (1+ c-beta))
                  
                  ;; Novo melhor encontrado, continua
                  (iterar-sucessores (cdr lista) prof novo-alfa beta jog f-suc f-aval t-lim ini val jogada novos-nos (if (> val alfa) (1+ c-alfa) c-alfa) c-beta)))
            
            ;; Valor nao melhora, continua
            (iterar-sucessores (cdr lista) prof alfa beta jog f-suc f-aval t-lim ini melhor-v melhor-j novos-nos c-alfa c-beta)))))