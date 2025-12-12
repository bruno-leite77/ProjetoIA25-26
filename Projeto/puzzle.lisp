;;; puzzle.lisp
;;; Modulo do Dominio: Tabuleiro, Pecas, Operadores e Heuristicas
;;; Implementacao conforme Ficha de Laboratorio 7 e Projeto

;;; ---------------------------------------------------------
;;; 1. SELETORES (Conforme Lab 7)
;;; ---------------------------------------------------------

(defun linha (l tabuleiro)
  "Retorna a linha l do tabuleiro (1-indexada)."
  (nth (1- l) tabuleiro))

(defun coluna (c tabuleiro)
  "Retorna uma lista com a coluna c do tabuleiro."
  (mapcar (lambda (lin) (nth (1- c) lin)) tabuleiro))

(defun celula (l c tabuleiro)
  "Retorna o valor na linha l e coluna c (1-indexado). Retorna nil se fora dos limites."
  (if (or (< l 1) (> l 7) (< c 1) (> c 7))
      nil
      (nth (1- c) (nth (1- l) tabuleiro))))

;;; ---------------------------------------------------------
;;; 2. FUNCOES AUXILIARES E MODIFICADORES (Conforme Lab 7)
;;; ---------------------------------------------------------

(defun celula-validap (l c tabuleiro)
  "Predicado: Verifica se a celula e jogavel (valor 0 ou 1)."
  (let ((val (celula l c tabuleiro)))
    (or (eql val 0) (eql val 1))))

(defun substituir-posicao (indice lista novo-valor)
  "Substitui o elemento no indice i de uma lista (1-indexado)."
  (if (null lista)
      nil
      (if (= indice 1)
          (cons novo-valor (cdr lista))
          (cons (car lista) (substituir-posicao (1- indice) (cdr lista) novo-valor)))))

(defun substituir (l c tabuleiro novo-valor)
  "Retorna um novo tabuleiro com a celula (l,c) alterada."
  (if (= l 1)
      (cons (substituir-posicao c (car tabuleiro) novo-valor) (cdr tabuleiro))
      (cons (car tabuleiro) (substituir (1- l) c (cdr tabuleiro) novo-valor))))

(defun aplicar-movimento (l c tabuleiro dl dc)
  "Aplica a logica de salto: Origem(1)->0, Meio(1)->0, Destino(0)->1."
  (let* ((l-dest (+ l dl))
         (c-dest (+ c dc))
         (l-meio (+ l (/ dl 2)))
         (c-meio (+ c (/ dc 2))))
    ;; Aplica as 3 alteracoes sequencialmente criando novos estados
    (substituir l-dest c-dest
                (substituir l-meio c-meio
                            (substituir l c tabuleiro 0)
                            0)
                1)))

;;; ---------------------------------------------------------
;;; 3. OPERADORES (Conforme Lab 7)
;;; Validam condicoes e retornam novo estado ou nil
;;; ---------------------------------------------------------

(defun operador-cd (l c tab)
  "Captura Direita: (l,c) salta para (l,c+2)"
  (if (<= (+ c 2) 7)
      (let ((orig (celula l c tab))
            (meio (celula l (1+ c) tab))
            (dest (celula l (+ c 2) tab)))
        (if (and (eql orig 1) (eql meio 1) (eql dest 0))
            (aplicar-movimento l c tab 0 2)
            nil))
      nil))

(defun operador-ce (l c tab)
  "Captura Esquerda: (l,c) salta para (l,c-2)"
  (if (>= (- c 2) 1)
      (let ((orig (celula l c tab))
            (meio (celula l (1- c) tab))
            (dest (celula l (- c 2) tab)))
        (if (and (eql orig 1) (eql meio 1) (eql dest 0))
            (aplicar-movimento l c tab 0 -2)
            nil))
      nil))

(defun operador-cc (l c tab)
  "Captura Cima: (l,c) salta para (l-2,c)"
  (if (>= (- l 2) 1)
      (let ((orig (celula l c tab))
            (meio (celula (1- l) c tab))
            (dest (celula (- l 2) c tab)))
        (if (and (eql orig 1) (eql meio 1) (eql dest 0))
            (aplicar-movimento l c tab -2 0)
            nil))
      nil))

(defun operador-cb (l c tab)
  "Captura Baixo: (l,c) salta para (l+2,c)"
  (if (<= (+ l 2) 7)
      (let ((orig (celula l c tab))
            (meio (celula (1+ l) c tab))
            (dest (celula (+ l 2) c tab)))
        (if (and (eql orig 1) (eql meio 1) (eql dest 0))
            (aplicar-movimento l c tab 2 0)
            nil))
      nil))

(defun operadores ()
  "Retorna a lista de funcoes operadores."
  (list #'operador-cd #'operador-ce #'operador-cc #'operador-cb))

;;; ---------------------------------------------------------
;;; 4. HEURISTICAS (Conforme Projeto)
;;; ---------------------------------------------------------

(defun contar-pinos (tabuleiro)
  "Conta o numero total de pinos (1s) no tabuleiro."
  (if (null tabuleiro)
      0
      (+ (count 1 (car tabuleiro)) 
         (contar-pinos (cdr tabuleiro)))))

(defun contar-jogadas (tab l c)
  "Conta recursivamente quantas jogadas sao possiveis no estado atual."
  (cond ((> l 7) 0) ;; Fim das linhas
        ((> c 7) (contar-jogadas tab (1+ l) 1)) ;; Proxima linha
        (t (+ (if (operador-cd l c tab) 1 0)
              (if (operador-ce l c tab) 1 0)
              (if (operador-cc l c tab) 1 0)
              (if (operador-cb l c tab) 1 0)
              (contar-jogadas tab l (1+ c))))))

(defun h1-base (estado)
  "Heuristica Base: h(x) = 1 / (jogadas_possiveis + 1).
   Privilegia estados com maior mobilidade."
  (let ((jogadas (contar-jogadas estado 1 1)))
    (/ 1.0 (1+ jogadas))))

(defun h2-extra (estado)
  "Heuristica Extra: Numero de Pinos - 1.
   Admissivel pois cada jogada remove exatamente 1 pino."
  (1- (contar-pinos estado)))