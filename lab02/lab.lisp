
;;;; laboratorio2.lisp
;;;; Disciplina de IA
;;;; Ficha de Laboratório nº2
;;;; Autor: Paulo Henrique Pereira

;;; Para Debug
;; soma-3
(defun soma-3 (a b c)
  (+ a b c))

# ; (soma-3)

;;; Exercicios sobre funções em Lisp (não recursivas)
;; notas-dos-alunos
(defun notas-dos-alunos ()
  '((15.5 15 8.25 13)(17.5 11 9 13.25)(11.75 0 0 16)))

# ; (notas-dos-alunos)

;; notas-do-primeiro-aluno
(defun notas-do-primeiro-aluno(notas-dos-alunos)
  (first notas-dos-alunos))

# ; (notas-do-primeiro-aluno (notas-dos-alunos))

#|
(defun notas-do-primeiro-aluno(notas-dos-alunos)
(first (funcall notas-dos-alunos)))

(notas-do-primeiro-aluno #'notas-dos-alunos)
|#

;; calcula-media-notas
(defun calcula-media-notas (notas)
  (/ (reduce #'+ notas) (length notas)))

# ; (calcula-media-notas '(1 2 3 4))
# ; (calcula-media-notas (notas-do-primeiro-aluno (notas-dos-alunos)))

;; maior-nota-do-aluno
(defun maior-nota-do-aluno (notas)
  (reduce #'max notas))

# ; (maior-nota-do-aluno (notas-do-primeiro-aluno (notas-dos-alunos)))
# ; (maior-nota-do-aluno '(1 2 3 4))
# ; (maior-nota-do-aluno #'notas-do-primeiro-aluno #'notas-dos-alunos)
