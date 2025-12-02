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
