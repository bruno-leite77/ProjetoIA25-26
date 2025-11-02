;; Projeto Jogo do Solitário
;; IA 2025/2026
;; Prof. Joaquim Filipe e Eng. Filipe Mariano
;; Autores:
;; António Guerreiro (202200160)
;; Bruno   Leite     (202100504)
;; Guilherme Cruz    (2024129841)


;; Menu Principal
(defun menu-Principal ()
  (format t "~% Bem-vindo ao Solitário! Escolha uma opção:")
  (format t "~% 1. Resolver um problema")
  (format t "~% 2. Sair")
  (format t "~% Opção > ")
  (let ((opcao (read)))
    (cond
      ((= opcao 1) (resolver-Problema))
      ((= opcao 2) (progn (format t "~% A sair...") (exit)))
      (t (progn (format t "~% Opção inválida! Tente novamente.") (main-Menu))))))

(defun resolver-Problema () )

;;==============================================================FUNÇÕES AUXILIARES DOS FICHEIROS=========================================================;

(defun pasta ()
  (let ((path "C:\\Users\\danie\\Downloads\\Projeto_IA_18_12\\Projeto"))
    path))

(defun diretorio (ficheiro)
  (concatenate 'string (pasta) ficheiro))


(defun iniciar-jogo ()
  (load (compile-file (diretorio "puzzle.lisp")))
  (load (compile-file (diretorio "procura.lisp")))
  (menu-Principal))


(defun carregar-problema (numero)
  (let ((problemas (ler-ficheiro (diretorio "\\problemas.dat"))))
    (nth (1- numero) problemas)))


(defun ler-ficheiro (nome-ficheiro)
  (with-open-file (stream nome-ficheiro :direction :input)
    (loop for linha = (read stream nil 'eof)
          until (eq linha 'eof)
          collect (mapcar (lambda (linha) (mapcar #'identity linha)) linha))))


