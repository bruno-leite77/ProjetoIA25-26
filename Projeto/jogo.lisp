;;; jogo.lisp
;;; Gestao de jogo e geracao de sucessores

;;; --- GERAÇÃO DE SUCESSORES (Sem Loop) ---

(defun gerar-todos-sucessores (tabuleiro jogador)
  "Gera lista de pares ((tipo l c) . novo-estado) recursivamente."
  (let ((todos-movimentos (gerar-sucessores-linhas 1 tabuleiro jogador)))
    (if (jogo-inicial-p tabuleiro)
        (filtrar-primeira-jogada todos-movimentos jogador)
        todos-movimentos)))

(defun gerar-sucessores-linhas (l tabuleiro jogador)
  (if (> l 7)
      nil
      (append (gerar-sucessores-colunas l 1 tabuleiro jogador)
              (gerar-sucessores-linhas (1+ l) tabuleiro jogador))))

(defun gerar-sucessores-colunas (l c tabuleiro jogador)
  (if (> c 7)
      nil
      (append (tentar-operadores l c tabuleiro jogador)
              (gerar-sucessores-colunas l (1+ c) tabuleiro jogador))))

(defun tentar-operadores (l c tab jogador)
  "Tenta aplicar todos os 8 operadores numa posicao usando MAPCAN."
  (let ((ops (list (list 'd #'operador-d) 
                   (list 'e #'operador-e) 
                   (list 'c #'operador-c) 
                   (list 'b #'operador-b)
                   (list 'cd #'operador-cd) 
                   (list 'ce #'operador-ce) 
                   (list 'cc #'operador-cc) 
                   (list 'cb #'operador-cb))))
    (mapcan (lambda (op-info)
              (let* ((nome (first op-info))
                     (funcao (second op-info))
                     (novo-estado (funcall funcao l c tab jogador)))
                (if novo-estado
                    (list (cons (list nome l c) novo-estado))
                    nil)))
            ops)))

;;; --- AUXILIARES ---

(defun contar-pecas (tab jogador)
  (if (null tab)
      0
      (+ (count jogador (car tab))
         (contar-pecas (cdr tab) jogador))))

(defun jogo-inicial-p (tabuleiro)
  "Verifica se é o estado inicial (simplificado)."
  (and (= (contar-pecas tabuleiro 1) 6)
       (= (contar-pecas tabuleiro 2) 6)))

(defun filtrar-primeira-jogada (movimentos jogador)
  "Regra: J1 so pode 'b', J2 so pode 'c'."
  (remove-if-not (lambda (par)
                   (let ((tipo (car (car par)))) 
                     (if (= jogador 1) (eq tipo 'b) (eq tipo 'c))))
                 movimentos))

;;; --- FUNCAO CAMPEONATO (Requisito 8) ---

(defun jogar (estado tempo-limite)
  "Funcao principal para o campeonato. Recebe estado e tempo (ms)."
  (let* ((pecas-j1 (contar-pecas estado 1))
         (pecas-j2 (contar-pecas estado 2))
         (jogador-atual (if (= pecas-j1 pecas-j2) 1 2)) 
         (inicio (get-internal-real-time)))
    
    (let ((resultado (negamax-alfa-beta estado 
                                        4 ; Profundidade
                                        -999999 
                                        999999 
                                        jogador-atual 
                                        #'gerar-todos-sucessores 
                                        #'avaliar-estado
                                        tempo-limite
                                        inicio)))
      (if (second resultado)
          (let ((melhor-jogada (second resultado))) 
             (list (car melhor-jogada) (cdr melhor-jogada)))
          nil))))