;; Projeto Jogo do Solitario
;; IA 2025/2026
;; Prof. Joaquim Filipe e Eng. Filipe Mariano
;; Autores:
;; Antonio Guerreiro (202200160)
;; Bruno   Leite     (202100504)
;; Guilherme Cruz    (2024129841)

;; Antes de comecar utilizar este comando para selecionar o diretorio do projeto
;; (hcl:change-directory "C:\\Users\\bruno\\Desktop\\ProjetoIA25-26\\Projeto")
;; (hcl:change-directory "C:\\Users\\bruno\\OneDrive\\Ambiente de Trabalho\\GitHub\\ProjetoIA25-26\\Projeto")
;; (load "projeto.lisp")


;; Carregar e Compilar Ficheiros
(compile-file "puzzle.lisp")
(load "puzzle")
(compile-file "procura.lisp")
(load "procura")

;; Variavel Global
(defparameter *lista-problemas* nil "Lista de tabuleiros carregada do ficheiro")

;;; ---------------------------------------------------------
;;; 1. LEITURA DE FICHEIROS E LOGS
;;; ---------------------------------------------------------

(defun ler-ficheiro (nome-ficheiro)
  (let ((ficheiro "problemas.dat"))
    (if (probe-file ficheiro)
        (with-open-file (in ficheiro :direction :input)
          (setf *lista-problemas* (loop for linha = (read in nil 'eof)
                      until (eq linha 'eof)
                      collect linha))
          (format t "~%Problemas carregados com sucesso.~%"))
        (format t "~%Erro: Ficheiro 'problemas.dat' nao encontrado.~%"))))

(defun escrever-log (resultado alg prob-id)
  (with-open-file (out "log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
    (if (null resultado)
        (format out "~%Data: ~a | Prob: ~d | Alg: ~a | Resultado: FALHOU" 
                (get-universal-time) prob-id alg)
        (let* ((no (first resultado))
               (g (second resultado))
               (e (third resultado))
               (tm (fourth resultado))
               (L (no-g no)) 
               (P (penetrancia L g))
               (B (ramificacao-media L g)))
          (format out "~%Data: ~a | Prob: ~d | Alg: ~a | Sol: ~d passos | Nos: ~d | Exp: ~d | T: ~d ms | Pen: ~5,3f | Ram: ~5,3f"
                  (get-universal-time) prob-id alg L g e tm P B)))))

;;; ---------------------------------------------------------
;;; 2. INTERFACE E VISUALIZACAO
;;; ---------------------------------------------------------

(defun imprimir-tabuleiro (tabuleiro)
  (dolist (linha tabuleiro)
    (format t " ~a~%" linha)))

(defun mostrar-solucao (resultado)
  (if (null resultado)
      (format t "~%Nenhuma solucao encontrada ou limite atingido.~%")
      (let* ((no-final (first resultado))
             (gerados (second resultado))
             (expandidos (third resultado))
             (tempo (fourth resultado))
             (caminho nil)
             (atual no-final))
        
        ;; Reconstruir caminho (tracar pai a pai)
        (loop while atual do
              (push atual caminho)
              (setf atual (no-pai atual)))
        
        (format t "~%--- SOLUCAO ENCONTRADA ---~%")
        (format t "Numero de passos: ~d~%" (1- (length caminho)))
        (format t "Estatisticas: Gerados: ~d | Expandidos: ~d | Tempo: ~d ms~%" gerados expandidos tempo)
        (format t "Penetrancia: ~5,3f | Ramificacao: ~5,3f~%" 
                (penetrancia (no-g no-final) gerados)
                (ramificacao-media (no-g no-final) gerados))
        
        (dolist (no caminho)
          (format t "Estado (g=~d, h=~5,3f):~%" (no-g no) (no-heuristica no))
          (imprimir-tabuleiro (no-estado no))
          (format t "--------------------~%"))
        
        (format t "Fim da solucao.~%"))))

;;; ---------------------------------------------------------
;;; 3. MENUS (Estrutura original preservada)
;;; ---------------------------------------------------------

(defun menu-principal ()
  (if (null *lista-problemas*)
      (ler-ficheiro "problemas.dat"))
 (loop
  (format t "~%  ================================================~%")
  (format t "  =  PROJETO SOLITÁRIO - INTELIGÊNCIA ARTIFICIAL =    ~%")
  (format t "  ================================================~%")
  (format t "~%  Escolha uma opção:")
  (format t "~% 1. Resolver um problema")
  (format t "~% 2. Sair")
  (format t "~% Opção > ")
  (let ((opcao (read)))
    (cond
     ((= opcao 1) (resolver-problema))
     ((= opcao 2) (format t "~% A sair...") (return))
     (t (format t "~% Opção inválida! Tente novamente."))))))

(defun resolver-problema ()
  (format t "~%Escolha o numero do problema (1 a ~d): " (length *lista-problemas*))
  (let ((n (read)))
    (if (and (integerp n) (>= n 1) (<= n (length *lista-problemas*)))
        ;; Bloco SE o numero for valido:
        (let ((tab-inicial (nth (1- n) *lista-problemas*)))
          (format t "~%Algoritmo:~%")
          (format t "1. BFS (Largura)~%")
          (format t "2. DFS (Profundidade)~%")
          (format t "3. A* (Heuristica Base: Mobilidade)~%")
          (format t "4. A* (Heuristica Extra: Contagem Pinos)~%")
          (format t "Opcao > ")
          
          (let ((alg (read)))
            (format t "~%A pensar...~%")
            (let ((resultado
                   (cond 
                     ((= alg 1) (bfs (criar-no tab-inicial)))
                     ((= alg 2) 
                      (format t "Profundidade Maxima: ")
                      (dfs (criar-no tab-inicial) (read)))
                     ((= alg 3) (a-star (criar-no tab-inicial) #'h1-base))
                     ((= alg 4) (a-star (criar-no tab-inicial) #'h2-extra))
                     (t nil))))
              
              (if resultado
                  (progn
                    (mostrar-solucao resultado)
                    (escrever-log resultado alg n))
                  (format t "~%Nao foi possivel encontrar solucao ou opcao invalida.~%")))))
        
        ;; Bloco SE o numero for invalido:
        (format t "~%Numero de problema invalido.~%"))))