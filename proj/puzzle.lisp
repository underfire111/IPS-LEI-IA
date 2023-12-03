
;; Board

(defparameter board (mount-board (shuffle-positions (list-positions))))
(defparameter positions-map (make-hash-table :test 'equal))
(defparameter score 0)

(defun list-positions ()
  (list "00" "01" "02" "03" "04" "05" "06" "07" "08" "09"
     "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
     "20" "21" "22" "23" "24" "25" "26" "27" "28" "29"
     "30" "31" "32" "33" "34" "35" "36" "37" "38" "39"
     "40" "41" "42" "43" "44" "45" "46" "47" "48" "49"
     "60" "61" "62" "63" "64" "65" "66" "67" "68" "69"
     "50" "51" "52" "53" "54" "55" "56" "57" "58" "59"
     "70" "71" "72" "73" "74" "75" "76" "77" "78" "79"
     "80" "81" "82" "83" "84" "85" "86" "87" "88" "89"
     "90" "91" "92" "93" "94" "95" "96" "97" "98" "99"))


(defun shuffle-positions (lst)
  (labels ((shuffle-helper (lst n)
             (cond ((<= n 1) lst)
		   (t (let* ((k (random n)) (temp (nth n lst)))
			(setf (nth n lst) (nth k lst))
			(setf (nth k lst) temp)
			(shuffle-helper lst (1- n)))))))
    (shuffle-helper lst (1- (length lst)))))


(defun mount-board (lst)
  (labels ((split-into-rows (lst row-size)
             (cond ((null lst) nil)
                 (t (cons (subseq lst 0 row-size)
                       (split-into-rows (nthcdr row-size lst) row-size))))))
    (cond ((and lst (= (length lst) 100)) (split-into-rows lst 10))
	  (t (error "Input list must contain 100 elements.")))))


(defun populate-map-positions (table)
  (dotimes (i 10)
    (dotimes (j 10)
      (let* ((value (nth j (nth i table)))
	      (position (list i j)))
	(setf (gethash value positions-map) position)))))

;;; (populate-map-positions board)


#####################################################################################

;; Utils

(defun clone-hash-table (table)
  "Creates a shallow copy of a hash table."
  (let ((new-table (make-hash-table
                    :test (hash-table-test table)
                    :size (hash-table-size table))))
    (maphash #'(lambda(key value)
                 (setf (gethash key new-table) value))
             table)
    new-table))

(defun reverse-string (str)
  "Reverse the characters in a string."
  (coerce (reverse (coerce str 'list)) 'string))

(defun join-numbers-as-string (num1 num2)
  "Join two numbers together as a string."
  (concatenate 'string (write-to-string num1) (write-to-string num2)))


#####################################################################################

;; Knight

(defparameter curr-state (make-hash-table :test 'equal))

(defun knight-start-position (x y)
  (let ((new-state (make-hash-table :test 'equal))
	 (value (nth x (nth y board))))
    (setf (gethash "kn" new-state) (list x y)) ;; set knight on board
    (setf (gethash (list x y) new-state) "#0") ;; add visited position
    (incf score (parse-integer value)) ;; add up to score
    (let ((temp (gethash (reverse-string value) positions-map)))
      (cond ((not (equal temp value))
	     (setf (gethash temp new-state) "#SYM"))
	    (t))) ;; else #DBL
    new-state))


(defun knight-can-move-to (current-state)
  (cond ((not (null (gethash "kn" current-state)))
	 (let ((x (first (gethash "kn" current-state)))
	       (y (second (gethash "kn" current-state)))
	       (moves '()))
	   (dolist (offset '((1 . 2) (2 . 1) (2 . -1) (1 . -2)
			     (-1 . -2) (-2 . -1) (-2 . 1) (-1 . 2)))
	     (let ((new-x (+ x (car offset)))
		   (new-y (+ y (cdr offset))))
	       (cond ((and (>= new-x 0) (<= new-x 9) (>= new-y 0) (<= new-y 9)
			(not (gethash (list new-x new-y) current-state)))
		   (push (list new-x new-y) moves))
		   (t (push nil moves)))))
	   (reverse moves)))))


(defun knight-can-move (current-state)
  (let ((next (knight-can-move-to current-state)))
    (not (notany #'identity next))))


(defun knight-validade-move (current-state move)
  (let ((next (knight-can-move-to current-state)))
    (and T (nth (- move 1) next))))


(defun knight-do-move (current-state move)
  (cond ((not (null current-state))
	 (let ((next (knight-validade-move current-state move)))
	   (cond (next
		  (let ((value (nth (first next) (nth (second next) board))))
		    (incf score (parse-integer value))
		    (setf (gethash "kn" current-state) next)
		    (setf (gethash next current-state) (format nil "#~a" move))
		    (let ((temp (gethash (reverse-string value) positions-map)))
		      (format t "~a ~a~%" value (reverse-string value))
		      (cond ((not (equal temp value))
			     (setf (gethash temp current-state) "#SYM"))
			    (t)))))
		 (t (error "Invalid move!")))
	   current-state))
	(t (error "Invalid state!"))))


;;; O estado de cada iteracoes ha de ser composto por uma lista de movimentos feitos e as respetivas casas
;;; Distancia percentual ao objetivo

(defstruct (node (:conc-name node-))
  current-state childs parent f g h)

(defun node-add-child (node child)
  (append (node-childs node) child))

(defun node-create (current-state parent f g h)
  (make-node :current-state current-state :parent parent :childs '() :f f :g g :h h))


#####################################################################################

;; Prints


(defun print-row(line)
  "Prints a string"
  (cond ((null line) nil)
        (t (format t "~a " (first line))
           (print-row (rest line)))))


(defun print-board(board)
  "Prints the board"
  (cond ((null board) nil)
        ((equal 'random (first board)) (format t "Random~%"))
        (t
         (print-row (first board))
         (format t "~%")
         (print-board (rest board)))))


(defun print-boards-info(boards)
  "Prints the informations about every board"
  (cond ((null boards) nil)
        ((stringp (first boards)) 
         (format t "Problem ~a:~%" (first boards))
         (print-boards-info (rest boards)))
        ((or (numberp (first boards)) (equal 'random (first boards))) 
         (format t "Objective: ~a~%" (first boards))
         (print-boards-info (rest boards)))
        (t
         (format t "Board:~%")
         (print-board (first boards))
         (format t  "-----------------------------~%")
         (print-boards-info (rest boards)))))


(defun print-boards(boards)
  "Print the list of all the boards available"
  (cond ((null boards) nil)
        (t (print-boards-info (first boards))
           (print-boards (rest boards)))))


(defun print-hash-map (tbl)
  "Print a map"
  (maphash #'(lambda (key value)
	       (format t "Key: ~a, Position: ~a~%" key value))
	   tbl))


(defun print-hash-map-sorted (tbl)
  "Print a map with its keys sorted"
  (let ((keys ()))
    (maphash #'(lambda (key value)
		 (declare (ignore value))
                 (push key keys))
             tbl)
    (dolist (key (sort keys 'string<))
      (format t "Key: ~a, Position: ~a~%" key (gethash key tbl)))))

