;;; 3. Interface e Apresentação (main.lisp)

(defun mostrar-solucao (no-solucao)
    "Apresenta o caminho da solução, do estado inicial ao final."

    ;; Função auxiliar recusiva para contruir o caminho na ordem correta
    (labels ((tracar-caminho (no)
                (if (null no)
                    nil ; Chegou à raiz
                    (append (tracar-caminho (no-pai no))
                            (list no)) ;; Adiciona o nó atual no fim
                )
            ))
        (let* ((caminho (tracar-caminho no-solucao))
               (passos (length caminho)))

            (format t "~%--- SOLUÇÃO ENCONTRADA ---~%")
            (format t "Comprimento do caminho: ~a passos~%~%" (1- passos))

            (dolist (no caminho)
                (format t "PASSO: ~a~%" (no-profundidade no))
                (format t "  Estado (g=~a, h=~a, f=~a):~%"
                        (no-profundidade no)
                        (no-heuristica no)
                        (no-custo no))

                ;; Imprimir o tabuleiro de forma legível
                (dolist (linha (no-estado no))
                    (format t "    ~a~%" linha))
                (format t "--------------------~%")
            )

            (format t "~%--- FIM DA SOLUÇÃO ---~%")
        )
    )
)

;;; 4. Dados e Menu Principal

;; 4.1. Definição do Tabuleiro Padrão (Inglês)
;; 1 = Pino, 0 = Vazio, NIL = Inválido
(defparameter *tabuleiro-solitario*
    '((nil nil 1 1 1 nil nil)
      (nil nil 1 1 1 nil nil)
      (1 1 1 1 1 1 1)
      (1 1 1 0 1 1 1) ; Centro vazio
      (1 1 1 1 1 1 1)
      (nil nil 1 1 1 nil nil)
      (nil nil 1 1 1 nil nil))
)

;; 4.2. Função do Menu Principal
(defun menu ()
    "Função principal para iniciar o programa."
    (format t "~%========================================~%")
    (format t "    PROJETO SOLITÁRIO - INTELIGÊNCIA ARTIFICIAL    ~%")
    (format t "========================================~%")
    (format t "Escolha o algoritmo de procura:~%")
    (format t "1. BFS (Procura em Largura)~%")
    (format t "2. DFS (Procura em Profundidade)~%")
    (format t "3. A* (A-Star) com Heurística 1 (Pinos Restantes)~%")    
)

;; HA MAIS

