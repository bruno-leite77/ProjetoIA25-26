;;; puzzle.lisp - Dominio Solitario 2 (Versao Final Otimizada)

;;; --- SELETORES E MODIFICADORES ---

(defun linha (l tabuleiro) (nth (1- l) tabuleiro))

(defun celula (l c tab)
  (if (or (< l 1) (> l 7) (< c 1) (> c 7)) nil (nth (1- c) (nth (1- l) tab))))

(defun substituir-posicao (i lista v)
  (cond ((null lista) nil) 
        ((= i 1) (cons v (cdr lista))) 
        (t (cons (car lista) (substituir-posicao (1- i) (cdr lista) v)))))

(defun substituir (l c tab v)
  (cond ((null tab) nil)
        ((= l 1) (cons (substituir-posicao c (car tab) v) (cdr tab)))
        (t (cons (car tab) (substituir (1- l) c (cdr tab) v)))))

(defun adversario (j) (if (= j 1) 2 1))

;;; --- OPERADORES FASE 1 (Salto sobre a própria peça) ---
(defun aplicar-movimento-f1 (l c tab dl dc)
  (let* ((ld (+ l dl)) (cd (+ c dc)) (lm (+ l (/ dl 2))) (cm (+ c (/ dc 2))))
    (substituir ld cd (substituir lm cm (substituir l c tab 0) 0) 1)))

(defun op-cd-f1 (l c tab) 
  (if (and (eql (celula l c tab) 1) (eql (celula l (1+ c) tab) 1) (eql (celula l (+ c 2) tab) 0))
      (aplicar-movimento-f1 l c tab 0 2) nil))
(defun op-ce-f1 (l c tab) 
  (if (and (eql (celula l c tab) 1) (eql (celula l (1- c) tab) 1) (eql (celula l (- c 2) tab) 0))
      (aplicar-movimento-f1 l c tab 0 -2) nil))
(defun op-cc-f1 (l c tab) 
  (if (and (eql (celula l c tab) 1) (eql (celula (1- l) c tab) 1) (eql (celula (- l 2) c tab) 0))
      (aplicar-movimento-f1 l c tab -2 0) nil))
(defun op-cb-f1 (l c tab) 
  (if (and (eql (celula l c tab) 1) (eql (celula (1+ l) c tab) 1) (eql (celula (+ l 2) c tab) 0))
      (aplicar-movimento-f1 l c tab 2 0) nil))

(defun operadores-f1 () (list #'op-cd-f1 #'op-ce-f1 #'op-cc-f1 #'op-cb-f1))

;;; --- OPERADORES FASE 2 (Movimento e Captura Adversario) ---
(defun aplicar-mov-f2 (l c tab j dl dc tipo)
  (let ((nl (+ l dl)) (nc (+ c dc)))
    (if (eq tipo :captura)
        (let ((ml (+ l (/ dl 2))) (mc (+ c (/ dc 2))))
          (substituir nl nc (substituir ml mc (substituir l c tab 0) 0) j))
        (substituir nl nc (substituir l c tab 0) j))))

(defun tentar-mov-f2 (l c tab j)
  (let ((ops '((d 0 1 nil) (e 0 -1 nil) (c -1 0 nil) (b 1 0 nil)
               (cd 0 2 t) (ce 0 -2 t) (cc -2 0 t) (cb 2 0 t))))
    (mapcan (lambda (o)
              (let* ((dl (second o)) (dc (third o)) (cap (fourth o))
                     (nl (+ l dl)) (nc (+ c dc)) (adv (adversario j)))
                (if (and (eql (celula l c tab) j) (eql (celula nl nc tab) 0))
                    (if cap
                        (if (eql (celula (+ l (/ dl 2)) (+ c (/ dc 2)) tab) adv)
                            (list (cons (list (car o) l c) (aplicar-mov-f2 l c tab j dl dc :captura))) nil)
                        (list (cons (list (car o) l c) (aplicar-mov-f2 l c tab j dl dc :simples))))
                    nil)))
            ops)))

;;; --- HEURISTICAS E FUNCOES AUXILIARES ---

(defun contar-pinos (tab) 
  (if (null tab) 0 (+ (count 1 (car tab)) (contar-pinos (cdr tab)))))

(defun h1-base (est) 
  (let ((pinos (contar-pinos est)))
    (if (zerop pinos) 0 (/ 1.0 (1+ pinos)))))

(defun h2-extra (estado)
  "Heuristica Extra: Numero de Pinos - 1."
  (let ((pinos (contar-pinos estado)))
    (if (> pinos 0) (1- pinos) 0)))

;;; --- AVALIACAO CORRIGIDA (EVITA LOOP) ---

(defun contar-pecas-jogador (tab j)
  (if (null tab) 0 
      (reduce #'+ (mapcar (lambda (lin) (count j lin)) tab))))

(defun pontuacao-posicional (tab j)
  "Dá mais pontos se as peças estiverem perto da base inimiga."
  (let ((pontos 0)
        (l 1))
    (dolist (linha tab)
      ;; Se for Jogador 1, quer ir para baixo (linhas maiores valem mais)
      ;; Se for Jogador 2, quer ir para cima (linhas menores valem mais, invertemos com 8-l)
      (let ((fator (if (= j 1) l (- 8 l))))
        (setf pontos (+ pontos (* (count j linha) fator))))
      (incf l))
    pontos))

(defun avaliar-estado (tab j)
  "Nova Heurística: Material (peso 50) + Posicional (peso 1)"
  (let* ((adv (adversario j))
         (pecas-eu (contar-pecas-jogador tab j))
         (pecas-ele (contar-pecas-jogador tab adv))
         ;; Valorizar muito ter mais peças que o adversário
         (score-material (* 50 (- pecas-eu pecas-ele))) 
         ;; Valorizar avançar no terreno (desempate)
         (score-posicional (- (pontuacao-posicional tab j) 
                              (pontuacao-posicional tab adv))))
    (+ score-material score-posicional)))