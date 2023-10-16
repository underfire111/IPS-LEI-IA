;;;; laboratorio4.lisp
;;;; Disciplina de IA
;;;; Laboratorio sobre funcoes recursivas e boas praticas de programacao em Lisp


;;; Exercicio Introdutorio.
;; comprimento de uma lista
(defun comprimento(lista)
"Recebe uma lista e retorna um valor inteiro que indica quantos elementos existem nesta lista"
  (cond
    ((null lista) 0)
    (t (1+ (comprimento (cdr lista))))
    )
  )
;; tamanho-das-sublistas
(defun tamanho-das-sublistas (lista)
"Recebe uma lista constituida por sublistas e retorna uma lista com valores inteiros que indicadam o comprimento das respetivas sublistas"
  (cond 
    ((null lista) nil)
    (T (cons (comprimento (car lista)) (tamanho-das-sublistas (cdr lista))))
  )
)

;;; Exercicio sobre funcoes recursivas

;; factorial

(defun factorial (num)
  (cond ((> num 0) (* num (factorial (- num 1))))
	(t 1)))

;; n-esimo

(defun n-esimo (n lst)
  (cond ((and (> n 0) (< n (length lst)))
	 (n-esimo (- n 1) (rest lst)))
	((= n 0) (first lst))
	(t nil)))

;; soma-lista

(defun soma-lista (lst)
  (cond ((> (length lst) 0)
	   (cond ((numberp (first lst))
		  (+ (first lst) (soma-lista (rest lst))))
		 (t (+ 0 (soma-lista (rest lst))))))
	(t 0)))
  
;; existe

(defun existe (num lst)
  (cond ((> (length lst) 0)
	 (cond ((equal (first lst) num) lst)
	       (t (existe num (rest lst)))))
	(t nil)))

;; junta

(defun junta (l1 l2)
  (cond ((null l1) l2)
	(t (cons (first l1) (junta (rest l1) l2)))))

;; inverte

(defun inverte (lst)
  (cond ((endp lst) lst)
	(t (junta (inverte (rest lst)) (list (first lst))))))


;; conta-atomos

(defun conta-atomos (lst)
  (cond ((endp lst) 0)
	(t (+ (cond ((atom (first lst)) 1)
		    (t (conta-atomos (first lst))))
	      (conta-atomos (rest lst))))))

;; alisa

(defun alisa (lst)
  (cond ((endp lst) '())
	(t (cond ((listp (first lst))
		  (junta (alisa (first lst)) (alisa (rest lst))))
		 (t (cons (first lst) (alisa (rest lst))))))))
