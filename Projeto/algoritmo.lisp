;;; algoritmo.lisp
;;; Implementacao generica do Negamax com cortes Alfa-Beta
;;; Sem variaveis globais, setf ou ciclos imperativos.

(defun negamax-alfa-beta (estado profundidade alfa beta jogador funcao-sucessores funcao-avaliacao tempo-limite inicio)
  "Algoritmo Negamax com cortes Alfa-Beta. Retorna: (valor melhor-jogada)"
  
  ;; 1. Verificar Tempo Limite
  (cond 
    ((>= (- (get-internal-real-time) inicio) tempo-limite)
     (list (funcall funcao-avaliacao estado jogador) nil))
    
    ;; 2. Verificar Profundidade Zero
    ((zerop profundidade)
     (list (* (funcall funcao-avaliacao estado jogador) 1) nil)) 
    
    (t
     (let ((sucessores (funcall funcao-sucessores estado jogador)))
       (if (null sucessores)
           ;; Estado terminal (sem jogadas)
           (list (funcall funcao-avaliacao estado jogador) nil)
           
           ;; 3. Processar sucessores recursivamente
           (processar-sucessores sucessores 
                                 profundidade 
                                 alfa 
                                 beta 
                                 jogador 
                                 funcao-sucessores 
                                 funcao-avaliacao 
                                 tempo-limite 
                                 inicio 
                                 -9999999  ; Melhor valor inicial (infinito negativo)
                                 nil)))))) ; Melhor jogada inicial

(defun processar-sucessores (lista-sucessores profundidade alfa beta jogador f-suc f-aval t-limite inicio melhor-valor melhor-jogada)
  "Funcao auxiliar recursiva para iterar sobre a lista de sucessores."
  
  (if (null lista-sucessores)
      (list melhor-valor melhor-jogada)
      
      (let* ((sucessor-atual (car lista-sucessores))
             (jogada (car sucessor-atual))      
             (novo-estado (cdr sucessor-atual))
             
             ;; Chamada recursiva Negamax (troca de sinal e limites)
             (resultado-filho (negamax-alfa-beta novo-estado 
                                                 (1- profundidade) 
                                                 (- beta) 
                                                 (- alfa) 
                                                 (adversario jogador) 
                                                 f-suc 
                                                 f-aval 
                                                 t-limite 
                                                 inicio))
             (valor-atual (- (first resultado-filho))))
        
        ;; Logica de Maximizacao e Corte
        (if (> valor-atual melhor-valor)
            (let ((novo-alfa (max alfa valor-atual)))
              (if (>= novo-alfa beta)
                  (list valor-atual jogada) ;; Corte Beta
                  
                  ;; Continua com novo melhor valor
                  (processar-sucessores (cdr lista-sucessores) 
                                        profundidade 
                                        novo-alfa 
                                        beta 
                                        jogador 
                                        f-suc 
                                        f-aval 
                                        t-limite 
                                        inicio 
                                        valor-atual 
                                        jogada)))
            
            ;; O valor nao melhorou, continua
            (processar-sucessores (cdr lista-sucessores) 
                                  profundidade 
                                  alfa 
                                  beta 
                                  jogador 
                                  f-suc 
                                  f-aval 
                                  t-limite 
                                  inicio 
                                  melhor-valor 
                                  melhor-jogada)))))

(defun adversario (jogador)
  "Retorna o ID do adversário (1 -> 2, 2 -> 1)"
  (if (= jogador 1) 2 1))