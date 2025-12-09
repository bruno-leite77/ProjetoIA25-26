;;; puzzle.lisp
;;; Modulo do Dominio: Tabuleiro, Pecas, Operadores e Heuristicas

;;; 1. Representacao e Seletores
;;; O tabuleiro e uma lista de listas. nil=invalido, 0=vazio, 1=pino.

(defun obter-celula (l c tabuleiro)
  "Retorna o valor na linha l e coluna c (1-indexado). Retorna nil se fora dos limites."
  (if (or (< l 1) (> l 7) (< c 1) (> c 7))
      nil
      (nth (1- c) (nth (1- l) tabuleiro))))

;;; 2. Modificadores de Estado (Funcional: cria novas copias)

(defun substituir-na-lista (lista indice novo-valor)
  "Auxiliar: Substitui o elemento no indice i de uma lista (recursivo)."
  (if (null lista)
      nil
      (if (= indice 0)
          (cons novo-valor (cdr lista))
          (cons (car lista) (substituir-na-lista (cdr lista) (1- indice) novo-valor)))))

(defun substituir-celula (l c tabuleiro novo-valor)
  "Retorna um novo tabuleiro com a celula (l,c) alterada."
  (if (= l 1)
      (cons (substituir-na-lista (car tabuleiro) (1- c) novo-valor) (cdr tabuleiro))
      (cons (car tabuleiro) (substituir-celula (1- l) c (cdr tabuleiro) novo-valor))))

(defun aplicar-movimento (l c tabuleiro dl dc)
  "Aplica um movimento generico: (l,c) -> salta sobre (l+dl/2, c+dc/2) -> destino (l+dl, c+dc)."
  (let* ((l-meio (+ l (/ dl 2)))
         (c-meio (+ c (/ dc 2)))
         (l-dest (+ l dl))
         (c-dest (+ c dc)))
    ;; O tabuleiro sofre 3 alteracoes: origem=0, meio=0, destino=1
    (substituir-celula l-dest c-dest
                       (substituir-celula l-meio c-meio
                                          (substituir-celula l c tabuleiro 0)
                                          0)
                       1)))

;;; 3. Operadores
;;; Cada operador verifica se o movimento e valido. Se sim, devolve novo estado. Se nao, devolve nil.

(defun operador-cd (l c tab)
  "Captura Direita: (l,c) salta sobre (l,c+1) para (l,c+2)"
  (if (<= (+ c 2) 7)
      (let ((orig (obter-celula l c tab))
            (meio (obter-celula l (1+ c) tab))
            (dest (obter-celula l (+ c 2) tab)))
        (if (and (eql orig 1) (eql meio 1) (eql dest 0))
            (aplicar-movimento l c tab 0 2)
            nil))
      nil))

(defun operador-ce (l c tab)
  "Captura Esquerda: (l,c) salta sobre (l,c-1) para (l,c-2)"
  (if (>= (- c 2) 1)
      (let ((orig (obter-celula l c tab))
            (meio (obter-celula l (1- c) tab))
            (dest (obter-celula l (- c 2) tab)))
        (if (and (eql orig 1) (eql meio 1) (eql dest 0))
            (aplicar-movimento l c tab 0 -2)
            nil))
      nil))

(defun operador-cc (l c tab)
  "Captura Cima: (l,c) salta sobre (l-1,c) para (l-2,c)"
  (if (>= (- l 2) 1)
      (let ((orig (obter-celula l c tab))
            (meio (obter-celula (1- l) c tab))
            (dest (obter-celula (- l 2) c tab)))
        (if (and (eql orig 1) (eql meio 1) (eql dest 0))
            (aplicar-movimento l c tab -2 0)
            nil))
      nil))

(defun operador-cb (l c tab)
  "Captura Baixo: (l,c) salta sobre (l+1,c) para (l+2,c)"
  (if (<= (+ l 2) 7)
      (let ((orig (obter-celula l c tab))
            (meio (obter-celula (1+ l) c tab))
            (dest (obter-celula (+ l 2) c tab)))
        (if (and (eql orig 1) (eql meio 1) (eql dest 0))
            (aplicar-movimento l c tab 2 0)
            nil))
      nil))

;;; 4. Heuristicas

(defun contar-pinos (tabuleiro)
  "Conta o numero total de pinos (1s) no tabuleiro."
  (if (null tabuleiro)
      0
      (+ (count 1 (car tabuleiro)) (contar-pinos (cdr tabuleiro)))))

(defun contar-movimentos-possiveis (tabuleiro l c)
  "Conta recursivamente quantas jogadas sao possiveis a partir de qualquer posicao."
  (cond ((> l 7) 0) ;; Fim do tabuleiro
        ((> c 7) (contar-movimentos-possiveis tabuleiro (1+ l) 1)) ;; Proxima linha
        (t (+ (if (operador-cd l c tabuleiro) 1 0)
              (if (operador-ce l c tabuleiro) 1 0)
              (if (operador-cc l c tabuleiro) 1 0)
              (if (operador-cb l c tabuleiro) 1 0)
              (contar-movimentos-possiveis tabuleiro l (1+ c))))))

(defun h1-base (estado)
  "Heuristica Base: h(x) = 1 / (jogadas_possiveis + 1). Privilegia mobilidade."
  (let ((jogadas (contar-movimentos-possiveis estado 1 1)))
    (/ 1.0 (1+ jogadas))))

(defun h2-extra (estado)
  "Heuristica Extra: Numero de Pinos. (Admissivel e util para A*)."
  (1- (contar-pinos estado)));; Código de Movimentos do Jogo
