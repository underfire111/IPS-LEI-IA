
; (entre-intervalo 5 '(0 10))

(defun entre-intervalo (x lst)
  (cond ((eq 2 (length lst))
	 (let ((m (reduce #'max lst)) (n (reduce #'min lst)))
	   (cond ((and (> x n) (< x m))      
		(format t "~d é maior que ~d e menor que ~d. ~C" x n m #\linefeed))
		(t (format t "~d não é um número entre ~d e ~d. ~C" x n m #\linefeed)))))
	(t (format t "A lista precisa de ter tamanho 2!!! ~C" #\linefeed))))



# ; (max-3 7 3 6)
(defun max-3(n1 n2 n3)
  (cond ((> n2 n1)
	 (setq n1 n2)))
  (cond ((> n3 n1)
	 (setq n1 n3)))
  n1
)

# ; (restop 10 5 0)
(defun restop(dividendo divisor resto)
  (cond ((eq divisor 0) nil)
	(t (let ((compare (mod dividendo divisor)))
	      (cond ((eq compare resto) T)
		    (t nil))))))

# ; (aprovadop '(13 15.6 5.5 7))
(defun aprovadop(list)
  (cond ((or (< 9.5 (first list)) (< 9.5 (fourth list))) nil))
  (let ((media (/ (reduce #'+ list) 4)))
    (cond ((>= media 9.5) T)
	  (t nil))))
	 

# ; (nota-final '(10 12 15) '(25 25 50))
(defun notas-validas (notas)
  (loop for x in notas
	do (cond ((and (>= x 0) (<= x 20)) T)
		 (t (format t "A nota ~d e invalida. ~C" x #\linefeed)
		    (return-from notas-validas NIL))))
  T)

(defun ponderacao-valida (pond)
  (cond ((equal 100 (reduce #'+ pond)) T)
	(t (format t "A soma das ponderacoes deve de ser igual a 100. ~C" #\linefeed))))

(defun aux-nota-final (notas pond index size)
  (cond ((>= index size) (return-from aux-nota-final 0))
	(t (return-from aux-nota-final
	     (+ (/ (* (nth index notas) (nth index pond)) 100)
		(aux-nota-final notas pond (+ 1 index) size))))))

(defun nota-final (notas pond)
  (cond ((= (length notas) (length pond))
	 (cond ((and (equal (notas-validas notas) T) (equal (ponderacao-valida pond) T))
		(aux-nota-final notas pond 0 (length notas)))
	       (t)))
	(t (format t "As listas tem tamanhos diferentes."))))

# ; (produto-somas '(1 2 3) '(2 2 2))

(defun aux-produto-somas (l1 l2 index size)
  (cond ((>= index size) (return-from aux-produto-somas 1))
	(t (return-from aux-produto-somas
	     (* (+ (nth index l1) (nth index l2)) (aux-produto-somas l1 l2 (+ 1 index) size))))))

(defun produto-somas (l1 l2)
  (cond ((= (length l1) (length l2)) (aux-produto-somas l1 l2 0 (length l1)))
	(t (format t "As listas tem tamanhos diferentes."))))

# ; (junta-listas-tamanho-igual '(1 3 4) '(5 3 2))

(defun junta-listas-tamanho-igual (l1 l2)
  (cond ((= (length l1) (length l2)) (append l1 l2))
	(t l2)))


# ; (dois-ultimos-elementos '(1 2 3 4 5 6 7))

(defun dois-ultimos-elementos (l1)
  (cond ((null l1) nil)
	(t (let ((size (length l1)))
	     (cond ((< size 2) nil)
		   ((= size 2) l1)
		   (t (subseq l1 (- size 2))))))))
	
# ; (palindromop '(1 2 3 2 1))

#| 
(defun aux-palindromop (l1 l2)
  (cond ((null l1) T)
	(t (and (equal (first l1) (first l2))
		(aux-palindromop (rest l1) (rest l2))))))

(defun palindromop (l1)
  (let ((l2 (reverse l1)))
    (aux-palindromop l1 l2)))
|#

(defun aux-palindromop (l1 l2 index size)
  (cond ((>= index size) T)
	(t (and (= (nth index l1) (nth index l2))
		(aux-palindromop l1 l2 (+ index 1) size)))))

(defun palindromop (l1)
  (let ((l2 (reverse l1)) (size (length l1)))
    (aux-palindromop l1 l2 0 (floor (/ size 2)))))

# ; (criar-pares '(1 2 3) '(4 5 6))

(defun aux-criar-pares (l1 l2)
  (cond ((null l1) nil)
	(t (cons (cons (first l1) (cons (first l2) nil))
		 (aux-criar-pares (rest l1) (rest l2))))))

(defun criar-pares (l1 l2)
  (cond ((and (and (not (null l1)) (not (null l2))) (equal (length l1) (length l2)))
	 (aux-criar-pares l1 l2))
	(t (format t "As listas sao invalidas."))))

# ; (verifica-pares '(1 2 3 4))

(defun verifica-pares (lst)
  (cond ((not (null lst))
	 (cond ((evenp (first lst))
		(cons T (verifica-pares (rest lst))))
	       (t (cons nil (verifica-pares (rest lst))))))))

# ; (rodar '(1 2 3 4) 'esq)

(defun esq (lst)
  (append (rest lst) (list (first lst))))

(defun dir (lst)
  (append (last lst) (butlast lst)))

(defun rodar (lst func)
  (funcall func lst))
