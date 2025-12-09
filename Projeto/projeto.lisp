;; Projeto Jogo do Solitário
;; IA 2025/2026
;; Prof. Joaquim Filipe e Eng. Filipe Mariano
;; Autores:
;; António Guerreiro (202200160)
;; Bruno   Leite     (202100504)
;; Guilherme Cruz    (2024129841)


"Para situar o diretório do jogo fazer: "
"(hcl:change-directory ""C:\\Users\\bruno\\Desktop\\ProjetoIA25-26\\Projeto"")"
;; Carregar Ficheiros
(load "puzzle.lisp")
(load "procura.lisp")

;; Variável Global
(defparameter *lista-problemas* nil "Lista de tabuleiros carregada do ficheiro")


;; Menu Principal
(defun menu-Principal ()
  (if (null *lista-problemas*)
      (ler-Ficheiro "problemas.dat")
  )
 
  (format t "~%========================================~%")
  (format t "    PROJETO SOLITÁRIO - INTELIGÊNCIA ARTIFICIAL    ~%")
  (format t "========================================~%")
  (format t "~%  Escolha uma opção:")
  (format t "~% 1. Resolver um problema")
  (format t "~% 2. Sair")
  (format t "~% Opção > ")
  (let ((opcao (read)))
    (cond
     ((= opcao 1) (resolver-Problema) (menu-Principal))
     ((= opcao 2) (format t "~% A sair..."))
     (t (progn (format t "~% Opção inválida! Tente novamente.") (menu-Principal)))))
)

;; Menu Secundário
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
                     (t nil)))) ;; Retorna nil se opcao invalida
              
              (if resultado
                  (mostrar-solucao resultado)
                  (format t "~%Nao foi possivel encontrar solucao ou opcao invalida.~%")))))
        
        ;; Bloco SE o numero for invalido (Else do IF inicial):
        (format t "~%Numero de problema invalido.~%")))) 
  ;; Fim da funcao

;; Funções Auxiliares

(defun ler-Ficheiro (nome-ficheiro)
  (let ((ficheiro "problemas.dat"))
    (if (probe-file ficheiro)
        (with-open-file (in ficheiro :direction :input)
          (setf *lista-problemas* (loop for linha = (read in nil 'eof)
                      until (eq linha 'eof)
                      collect linha))
          (format t "~%Problemas carregados com sucesso.~%"))
        (format t "~%Erro: Ficheiro 'problemas.dat' nao encontrado.~%")))
)

(defun imprimir-Tabuleiro (tabuleiro)
  (dolist (linha tabuleiro)
    (format t " ~a~%" linha))
)

(defun mostrar-solucao (no-solucao)
  (if (null no-solucao)
      (format t "~%Nenhuma solucao encontrada ou limite atingido.~%")
      (let ((caminho nil)
            (atual no-Solucao))
        ;; Reconstruir caminho (tracar pai a pai)
        (loop while atual do
              (push atual caminho)
              (setf atual (no-pai atual)))
        
        (format t "~%--- SOLUCAO ENCONTRADA ---~%")
        (format t "Numero de passos: ~d~%" (1- (length caminho)))
        
        (dolist (no caminho)
          (format t "Estado (g=~d, h=~5,3f):~%" (no-g no) (no-h no))
          (imprimir-Tabuleiro (no-estado no))
          (format t "--------------------~%"))
        
        (format t "Fim da solucao.~%")))
)



