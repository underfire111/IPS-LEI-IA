;;;; laboratorio5.lisp
;;;; Ficha laboratorial sobre funcoes de alto nivel em Lisp
;;;; Autor: 


;;; Exercicio Introdutorio  - funcall + lambda
;;(remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4))
(defun remover-se(pred lista)
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))


;;; Exercicios - Parametros de lambda
;;(media 1 2 34 )
;; 12.333333

(defun media (&rest b)
  (/ (reduce #'+ b) (length b)))


;; (coluna ((1 2 3) (4 5 6) (7 8 9)))
;; (1 4 7)

(defun coluna (m1)
  (mapcar #'(lambda (row &aux (head (first row)))
	      head)
	  m1))

;; (aplicar-simbolo 'mod  2 3)
;; 2
;; (aplicar-simbolo '*  2 3)
;; 6

(defun aplicar-simbolo (&rest expression)
  (eval expression))

;;; Exercicio avaliacao de turmas de alunos

;; Teste: (turma-1)
;; (("Joao" "Silva" (12.5 15 8.5)) ("Ana" "Santos" (11.5 18 13.5)) ("Paulo" "Jorge" (6.5 10 7.5)) ("Elisabete" "Navarro" (12.5 15 8.5)) ("Mario" "Rodrigues" (12.5 15 8.5)))

(defun turma-1 ()
  '(("Joao" "Silva" (12.5 15 8.5))
   ("Ana" "Santos" (11.5 18 13.5))
   ("Paulo" "Jorge" (6.5 10 7.5))
   ("Elisabete" "Navarro" (12.5 15 8.5))
   ("Mario" "Rodrigues" (12.5 15 8.5))))

;; nome

(defun nome (table &rest args)
  (cond ((null args) (first table))
	(t (let ((row (first args)))
		 (cond ((and (integerp row) (>= row 0) (< row (length table)))
			(first (nth row table)))
		       (t nil))))))

;; apelido

(defun apelido (table &rest args)
  (cond ((null args) (second table))
	(t (let ((row (first args)))
		 (cond ((and (integerp row) (>= row 0) (< row (length table)))
			(second (nth row table)))
		       (t nil))))))


;; notas

(defun notas (tbl &rest args)
  (cond ((null args) (third tbl))
	(t (let ((row (first args)))
		 (cond ((and (integerp row) (>= row 0) (< row (length tbl)))
			(third (nth row tbl)))
		       (t nil))))))


;; media-das-notas
;; Teste: (media-das-notas '(10 15 20))
;; 15

(defun media-das-notas (lst)
  (let ((size (length lst)))
    (cond ((> size 0) (/ (apply #'+ lst) size)))))

;; media-da-turma
;; (media-da-turma #'media-notas (turma-1))
;; 11.666666

(defun alisa (lst)
  (cond ((endp lst) '())
	(t (cond ((listp (first lst))
		  (append (alisa (first lst)) (alisa (rest lst))))
		 (t (cons (first lst) (alisa (rest lst))))))))

(defun media-da-turma (ft tbl)
  (cond ((endp tbl) '())
	(t (let ((grades (alisa (mapcar #'third tbl))))
	   (apply ft (list grades))))))

;; percentagem-de-aprovados
;; (percentagem-aprovados (turma-1))
;; 80.0

(defun approvedp (grade)
  (cond ((>= grade 9.5) T)
	(t nil)))

(defun contagem-aprovados (tbl)
  (let ((grades (apply #'(lambda (lst) (mapcar #'media-das-notas lst)) (list (mapcar #'third tbl)))))
    (count 'T (mapcar #'approvedp grades))))

(defun percentagem-aprovados (tbl)
  (let ((size (length tbl))
	(count (contagem-aprovados tbl)))
    (* (/ count size) 100.0)))

;; lista-dos-aprovados
;; Teste: (lista-dos-aprovados (turma-1))
;; Teste avaliar-turma: (avaliar-turma 'lista-dos-aprovados (turma-1))
;; (("Joao" "Silva") ("Ana" "Santos") NIL ("Elisabete" "Navarro") ("Mario" "Rodrigues"))

(defun aux-lista-dos-aprovados (tbl))

(defun lista-dos-aprovados (tbl)
  (let ((grades (apply #'(lambda (lst) (mapcar #'media-das-notas lst)) (list (mapcar #'third tbl)))))
    ( (mapcar #'approvedp grades))))

;; todos-aprovadosp
;; (todos-aprovadosp (turma-1))
;; NIL

(defun todos-aprovadosp (tbl)
  (cond ((= (percentagem-aprovados tbl) 100) T)
	(t nil)))

;;avaliar-turma
;;(("Joao" "Silva" (12.5 15 8.5)) ("Ana" "Santos" (11.5 18 13.5)) NIL ("Elisabete" "Navarro" (12.5 15 8.5)) ("Mario" "Rodrigues" (12.5 15 8.5)))
;; (avaliar-turma (turma-1) 'media-da-turma)
;; 11.6666
;; (avaliar-turma (turma-1) 'percentagem-aprovados )
;; 80.0


;; existep
;; (existep "Joao" "Silva" (turma-1)
;; T


