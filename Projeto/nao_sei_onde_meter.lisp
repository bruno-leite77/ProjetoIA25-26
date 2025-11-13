;;; 1. Domínio de Aplicação (solitaire.lisp)
;; 1.1. Seletores e Funções Auxiliares

(defun linha (l tabuleiro)
    "Retorna a linha (l) 1-indexada do tabuleiro."
    (nth (- l 1) tabuleiro)
)

(defun celula (l c tabuleiro)
    "Retorna o valor da célula (l, c) 1-indexada."
    (if (or (< l 1) (> l 7) (< c 1) (> c 7))
        nil ; Fora dos limites
        (nth (- c 1) (nth (- l 1) tabuleiro))
    )
)

(defun celula-validap (l c tabuleiro)
    "Verifica se a célula (l, c) é jogável (0 ou 1)."
    (let ((val (celula l c tabuleiro)))
        (if (or (= val 0) (= val 1))
            t
            nil
        )
    )
)

;; 1.2. Modificação de Estado

(defun substituir-posicao (idx lista valor)
    "Retorna nova lista com a posição (idx) 1-indexada substituída."
    (if (null lista)
        nil
        (if (= idx 1)
            (cons valor (cdr lista))
            (cons (car lista) (substituir-posicao (- idx 1) (cdr lista) valor))
        )
    )
)

(defun substituir (l c tabuleiro valor)
    "Retorna novo tabuleiro com a célula (l, c) 1-indexada substituída."
    (if (null tabuleiro)
        nil
        (if (= l 1)
            (cons (substituir-posicao c (car tabuleiro) valor) (cdr tabuleiro))
            (cons (car tabuleiro) (substituir (- l 1) c (cdr tabuleiro) valor))
        )
    )
)

;; 1.3. Operadores

(defun operador-cd (l c tab)
    "Aplicar 'Captura Direita' na peça (l, c). Retorna o novo estado ou NIL se inválido."
    
    ;; Validar limites antes de aceder à célula
    (if (> (+ c 2) 7) (return-from operador-cd nil))

    (let ((origem (celula l c tab))
          (meio (celula l (+ c 1) tab))
          (destino (celula l (+ c 2) tab)))

        ;; Validar condições da captura
        (if (and (= origem 1) (= meio 1) (= destino 0))
            ;; Aplicar movimento
            (let* ((tab1 (substituir l c tab 0))         ; Esvazia origens
                   (tab2 (substituir l (+ c 1) tab1 0))  ; Remove pino meio
                   (tab3 (substituir l (+ c 2) tab2 1))) ; Move pino
                tab3)
            ;; Movimento inválido
            nil
        )

    )
)

(defun operador-ce (l c tab)
    "Aplica 'Captura Esquerda' na peça (l, c). Retorna o novo estado ou NIL se inválido"
    (if (< (- c 2) 1) (return-from operador-ce nil))

    (let ((origem (celula l c tab))
          (meio (celula l (- c 1) tab))
          (destino (celula l (- c 2) tab)))

        (if (and (= origem 1) (= meio 1) (= destino 0))
            (let* ((tab1 (substituir l c tab 0))
                   (tab2 (substituir l (- c 1) tab1 0))
                   (tab3 (substituir l (- c 2) tab2 1)))
                tab3)
            nil
        )
    )
)

(defun operador-cc (l c tab)
    "Aplica 'Captura Cima' na peça (l, c). Retorna o novo estado ou NIL se inválido"
    (if (< (- l 2) 1) (return-from operador-cc nil))

    (let ((origem (celula l c tab))
          (meio (celula (- l 1) c tab))
          (destino (celula (- l 2) c tab)))

        (if (and (= origem 1) (= meio 1) (= destino 0))
            (let* ((tab1 (substituir l c tab 0))
                   (tab2 (substituir (- l 1) c tab1 0))
                   (tab3 (substituir (- l 2) c tab2 1)))
                tab3)
            nil
        )
    )
)

(defun operador-cb (l c tab)
    "Aplica 'Captura Baixo' na peça (l, c). Retorna o novo estado ou NIL se inválido"
    (if (> (+ l 2) 7) (return-from operador-cb nil))

    (let ((origem (celula l c tab))
          (meio (celula (+ l 1) c tab))
          (destino (celula (+ l 2) c tab)))

        (if (and (= origem 1) (= meio 1) (= destino 0))
            (let* ((tab1 (substituir l c tab 0))
                   (tab2 (substituir (+ l 1) c tab1 0))
                   (tab3 (substituir (+ l 2) c tab2 1)))
                tab3)
            nil
        )
    )
)

;;; 2. Procura Genérica (search.lisp)
;; 2.1. Estrutura do Nó

(defun no-estado (no)
    "Retorna o estado do nó."
    (first no)
)

(defun no-profundidade (no)
    "Retorna a profundidade do nó."
    (second no)
)

(defun no-pai (no)
    "Retorna o nó pai."
    (third no)
)

;; 2.2. Funções Auxiliares

(defun contar-pinos (tabuleiro)
    "Função auxiliar para contar pinos (1s) no estado."
    (if (null tabuleiro)
        0
        (+ (count 1 (car tabuleiro))
           (contar-pinos (cdr tabuleiro)))
    )
)

(defun no-solucaop (tabuleiro)
    "Predicado: Verifica se o nó é solução (1 pino)."
    (= (contar-pinos (no-estado no )) 1)
)

(defun no-existep (no lista-nos)
    "Predicado: Verifica se o ESTADO do nó existe na lista."
    (if (null lista-nos)
        nil
        (if (equal (no-estado no) (no-estado (car lista-nos)))
            t
            (no-existep no (cdr lista-nos))
        )
    )
)

;; 2.3. Gestão de Listas

(defun abertos-bfs (abertos sucessores)
    "FIFO: Adiciona sucessores ao FIM da lista."
    (append abertos sucessores)
)

(defun aberto-fds (abertos sucessores)
    "LIFO: Adiciona sucessores ao INÍCIO da lista."
    (append sucessores abertos)
)
