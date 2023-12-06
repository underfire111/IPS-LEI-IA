;; positions-map	| key: "value"  value: (z y)
;; state-map		| key: (x y)    value: "#CODE" 

;; ### Board ##################################################################################

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


(defun populate-positions-map (table)
  (dotimes (i 10)
    (dotimes (j 10)
      (let* ((value (nth j (nth i table)))
	     (position (list j i)))
	(if (not (null value))
	    (setf (gethash value positions-map) position))))))


;; ### Utils ##################################################################################

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

;;; (defun get-doubles-available)


;; ### Knight #################################################################################

(defun knight-start-position (x y)
  "Places the knight at the board"
  (let ((new-state (make-hash-table :test 'equal))
	 (value (nth x (nth y board))))
    (setf (gethash "kn" new-state) (list x y)) ;; set knight on board
    (setf (gethash (list x y) new-state) "#0") ;; add visited position
    (let ((temp (gethash (reverse-string value) positions-map)))
      (cond ((not (equal temp value))
	     (setf (gethash temp new-state) "#SYM"))
	    (t))) ;; else #DBL
    new-state))


(defun knight-can-move-to (current-state)
  "Check for all possible moves."
  (cond ((not (null (gethash "kn" current-state)))
	 (let ((x (first (gethash "kn" current-state)))
	       (y (second (gethash "kn" current-state)))
	       (moves '()))
	   (dolist (offset '((1 . 2) (2 . 1) (2 . -1) (1 . -2) (-1 . -2) (-2 . -1) (-2 . 1) (-1 . 2)))
	     (let ((new-x (+ x (car offset))) (new-y (+ y (cdr offset))))
	       (cond ((and (>= new-x 0) (<= new-x 9) (>= new-y 0) (<= new-y 9)
			   (not (gethash (list new-x new-y) current-state))
			   (nth new-x (nth new-y board)))
		   (push (list new-x new-y) moves))
		   (t (push nil moves)))))
	   (reverse moves)))))


(defun knight-can-move (current-state)
  "Check if the knight still have any move available."
  (let ((next (knight-can-move-to current-state)))
    (not (notany #'identity next))))


(defun knight-move-to (current-state coordinates)
  (cond ((not (null current-state))
	 (cond (coordinates
		(let ((value (nth (first coordinates) (nth (second coordinates) board))))
		  (setf (gethash "kn" current-state) coordinates)
		  (setf (gethash coordinates current-state) "#MOV")
		  (let ((temp (gethash (reverse-string value) positions-map)))
		    (cond ((not (equal temp value))
			   (setf (gethash temp current-state) "#SYM"))
			  (t))))) ;; DBL
	       (t (error "Invalid move!")))
	 current-state)
	(t (error "Invalid state!"))))


(defun knight-do-move (current-state next-generation move)
  "Execute a valid knight move"
  (cond ((not (null current-state))
	 (let ((next (nth move next-generation)))
	   (cond (next
		  (let ((value (nth (first next) (nth (second next) board))))
		    (setf (gethash "kn" current-state) next)
		    (setf (gethash next current-state) (format nil "#~a" move))
		    (let ((temp (gethash (reverse-string value) positions-map)))
		      (format t "~a ~a~%" value (reverse-string value))
		      (cond ((not (equal temp value))
			     (setf (gethash temp current-state) "#SYM"))
			    (t))))) ;; DBL
		 (t (error "Invalid move!")))
	   current-state))
	(t (error "Invalid state!"))))

d
;;; (defparameter current-state (knight-start-position 0 0))

;; so para confirmar
;; node e uma lista com:
;; curr-state cur-score parent childs fgh
;;     1          2       3      4     5
;; ((1 0) (2 3) (3 4))
;; ((1 0) (2 3) (3 4))


(defun create-node (current-state &optional parent)
  (let* ((coordinates (gethash "kn" current-state))
	 (score (parse-integer (nth (first coordinates) (nth (second coordinates) board)))))
    (list current-state
	  (if (not (null parent)) (+ score (second parent)) score)
	  (if (not (null parent)) parent nil)
	  '())))


(defun partenogenese (node &optional calculate_fgh)
  (let ((possible-moves (knight-can-move-to (first node))))
    (mapcar #'(lambda (coordinates)
		"Creates a new node with the coordinates given"
		(if (not (null coordinates))
		    (let* ((child-state (knight-move-to (clone-hash-table (first node)) coordinates))
			   (child-node (create-node child-state node)))
		      (if (not (null calculate_fgh)) (funcall calculate_fgh child-node node))
		      (if (null (fourth node)) (setf (fourth node) child-node) (append (fourth node) child-node))
		      child-node)))
	    possible-moves)))


