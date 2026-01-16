;;; projeto.lisp
;;; Ficheiro Principal - Integracao Fase 1 e Fase 2

;; AJUSTAR CAMINHO
;;(hcl:change-directory "C:/Users/guilh/OneDrive/Documentos/Escolinha/IA/Projeto/Projeto/")
(hcl:change-directory "C:/Users/bruno/Desktop/ProjetoIA25-26/Projeto")
(load "puzzle.lisp")
(load "procura.lisp")
(load "algoritmo.lisp")
(load "jogo.lisp")

(defparameter *lista-problemas* nil "Lista de tabuleiros carregada do ficheiro")

;;; ---------------------------------------------------------
;;; 2. GESTAO DE FICHEIROS
;;; ---------------------------------------------------------

(defun ler-ficheiro (nome-ficheiro)
  "Le o ficheiro problemas.dat e carrega a variavel global."
  (let ((ficheiro (if (probe-file nome-ficheiro) nome-ficheiro "problemas.dat")))
    (if (probe-file ficheiro)
        (with-open-file (in ficheiro :direction :input)
          (setf *lista-problemas* (loop for linha = (read in nil 'eof)
                      until (eq linha 'eof)
                      collect linha))
          (format t "~%Sucesso: ~d problemas carregados.~%" (length *lista-problemas*)))
        (format t "~%Erro: Ficheiro '~a' nao encontrado.~%" ficheiro))))

(defun escrever-log (resultado alg prob-id)
  "Escreve os resultados no ficheiro log.dat (Fase 1)."
  (with-open-file (out "log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
    (if (null resultado)
        (format out "~%Data: ~a | Prob: ~d | Alg: ~a | Resultado: FALHOU" 
                (get-universal-time) prob-id alg)
        (let* ((no (first resultado))
               (g (second resultado)) (e (third resultado)) (tm (fourth resultado))
               (L (no-g no)) (P (penetrancia L g)) (B (ramificacao-media L g)))
          (format out "~%Data: ~a | Prob: ~d | Alg: ~a | Sol: ~d | Nos: ~d | Exp: ~d | T: ~d ms | Pen: ~5,3f | Ram: ~5,3f"
                  (get-universal-time) prob-id alg L g e tm P B)))))

;;; ---------------------------------------------------------
;;; 3. INTERFACE E VISUALIZACAO
;;; ---------------------------------------------------------

(defun imprimir-tabuleiro (tabuleiro)
  (dolist (linha tabuleiro)
    (format t " ~a~%" linha)))

(defun mostrar-solucao (resultado)
  "Mostra a solucao da Fase 1 (Caminho de estados)."
  (if (null resultado)
      (format t "~%Nenhuma solucao encontrada.~%")
      (let* ((no-final (first resultado))
             (caminho nil)
             (atual no-final))
        ;; Reconstruir caminho (tracar pai a pai)
        (loop while atual do
              (push atual caminho)
              (setf atual (no-pai atual)))
        
        (format t "~%--- SOLUCAO ENCONTRADA ---~%")
        (format t "Passos: ~d | Gerados: ~d | Expandidos: ~d | Tempo: ~d ms~%" 
                (1- (length caminho)) (second resultado) (third resultado) (fourth resultado))
        
        (dolist (no caminho)
          (format t "G=~d H=~5,3f~%" (no-g no) (no-heuristica no))
          (imprimir-tabuleiro (no-estado no))
          (format t "--------------------~%")))))

;;; ---------------------------------------------------------
;;; 4. MENUS
;;; ---------------------------------------------------------

(defun menu-principal ()
  "Menu Principal do Projeto (Fase 1 e 2)."
  ;; Tenta carregar problemas se a lista estiver vazia
  (if (null *lista-problemas*) (ler-ficheiro "problemas.dat"))
  
  (format t "~%===============================================~%")
  (format t "   PROJETO SOLITARIO - IA 2025/2026            ~%")
  (format t "===============================================~%")
  (format t " 1. Fase 1: Resolver Problema (1 Jogador)~%")
  (format t " 2. Fase 2: Multijogador (2 Jogadores)~%")
  (format t " 0. Sair~%")
  (format t "Opcao > ")
  
  (let ((opcao (read)))
    (cond
      ((= opcao 1) (menu-fase1) (menu-principal))
      ((= opcao 2) (menu-fase2) (menu-principal))
      ((= opcao 0) (format t "~%A sair... Adeus!~%"))
      (t (format t "~%Opcao invalida.~%") (menu-principal)))))

(defun menu-fase1 ()
  "Sub-menu para a Fase 1 (Procura)."
  (format t "~%--- FASE 1: PROCURA ---~%")
  (format t "Escolha o problema (1 a ~d): " (length *lista-problemas*))
  (let ((n (read)))
    (if (and (integerp n) (>= n 1) (<= n (length *lista-problemas*)))
        (let ((tab-inicial (nth (1- n) *lista-problemas*)))
          (format t "Algoritmo:~%1. BFS~%2. DFS~%3. A* (Base)~%4. A* (Extra)~%Opcao > ")
          (let ((alg (read)))
            (format t "~%A resolver... (Aguarde)~%")
            (let ((resultado
                   (cond 
                     ((= alg 1) (bfs (criar-no tab-inicial)))
                     ((= alg 2) (format t "Profundidade Max: ") (dfs (criar-no tab-inicial) (read)))
                     ((= alg 3) (a-star (criar-no tab-inicial) #'h1-base))
                     ((= alg 4) (a-star (criar-no tab-inicial) #'h2-extra))
                     (t nil))))
              (if resultado
                  (progn (mostrar-solucao resultado) (escrever-log resultado alg n))
                  (format t "~%Falha ou opcao invalida.~%")))))
        (format t "~%Problema invalido.~%"))))

(defun tabuleiro-oficial ()
  "Retorna o tabuleiro inicial padrao do Solitario 2."
  '((nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (nil nil 2 2 2 nil nil)
    (nil nil 2 2 2 nil nil)))

(defun menu-fase2 ()
  (format t "~%--- JOGO SOLITARIO 2 (Versao Oficial) ---~%")
  
  ;; Em vez de pedir para escolher, carregamos logo o oficial:
  (let ((tab (tabuleiro-oficial)))
    (format t "~%Tabuleiro Inicial Carregado.~%")
    
    (format t "~%1. Humano vs Computador")
    (format t "~%2. Computador vs Computador")
    (format t "~%Opcao > ")
    (let ((op (read)))
      (cond
        ;; HUMANO vs PC
        ((= op 1)
         (format t "Quem joga com as pecas de CIMA (Jogador 1)?")
         (format t "~%1. Humano (Tu)")
         (format t "~%2. Computador")
         (format t "~%Opcao > ")
         (let ((quem (read)))
           (format t "Tempo limite para o PC (ms): ")
           (let ((tempo (read)))
             (if (= quem 1)
                 (ciclo-jogo tab 1 'humano 'pc tempo)
                 (ciclo-jogo tab 1 'pc 'humano tempo)))))
        
        ;; PC vs PC
        ((= op 2)
         (format t "Tempo limite por jogada (ms): ")
         (let ((tempo (read)))
           (ciclo-jogo tab 1 'pc 'pc tempo)))
        
        (t (format t "Opcao invalida.")))))
  
  ;; Se quiseres voltar ao menu principal no fim, descomenta a linha abaixo:
  ;; (menu-principal)
  )