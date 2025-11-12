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

    
)
