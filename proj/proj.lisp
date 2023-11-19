

#####################################################################################

;; Loader

(defun get-number()
  "Receives an input from the user and guarantees that it is a number"
  (let ((value (read)))
      (if (numberp value) (return-from get-number value)))
  (print "The answer must be a number!
Please choose a number as an option:") 
  (get-number))


(defun load-boards(filename)
  "Reads the filename and loads every board in it"
  (let ((result '())
        (current_problem '())
        (current_board '()))
    (with-open-file (file filename :direction :input :if-does-not-exist :error)
      (loop for line = (read-line file nil)
            while line
            do
	       (cond ((char= #\P (char line 0))
                     (setf current_problem (list (subseq line (1+ (position #\Space line)) (1- (length line)))))) ;; Gets the problem's name
                    ((char= #\O (char line 0)) ;; Gets its objective
                     (push (get-objective line) current_problem))
                    ((char= #\- (char line 0)) ;; Separator between problems
                     (push (reverse (copy-list current_board)) current_problem) ;; stores current problem
                     (push (reverse (copy-list current_problem)) result)
                     (setf current_board '()) ;; Resets variables
                     (setf current_problem '()))
                    (t (push (get-board line) current_board))))) ;; Gets the current board
    (reverse result)))


(defun get-objective(line)
  "Parses the objetive to integer if it is a number, else returns random"
  (let ((objective (subseq line (position #\Space line) (length line))))
    (cond ((char= #\? (char objective 1)) 'random)
          (t (parse-integer objective)))))


(defun get-board(line)
  "Transforms the current line of the board into a list. If the line says random, it will return the string random"
  (cond ((char= #\r (char line 0)) 'random)
        (t 
         (loop for start = 0 then (1+ end)
               for end = (or (position #\Space line :start start) (1- (length line)))
               while (and end (< start (length line)))
               collect (let ((c (subseq line start (1+ end))))
                         (cond ((or (equal c "NIL") (equal c "NIL ")) nil)
                               (t (parse-integer c))))))))


#####################################################################################

;; Board

(defparameter positions-map (make-hash-table))
(defparameter score 0)
(defparameter current-position)

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

(defun print-map-keys-sorted ()
  (let ((keys ()))
    (maphash #'(lambda (key value)
		 (declare (ignore value))
                 (push key keys))
             positions-map)
    (dolist (key (sort keys 'string<))
      (format t "Key: ~a, Position: ~a~%" key (gethash key positions-map)))))



(defun reverse-string (str)
  (coerce (reverse (coerce str 'list)) 'string))

;;; (populate-map-positions (mount-board (shuffle-positions (list-positions))))
;;; (print-map-keys-sorted)

#####################################################################################

;; Knight

(defun knight-set-position (tbl x y)
  (cond ((and (and (>= x 0) (< x 10)) (and (>= y 10) (< y 10)))
	 (setf ((nth x (nth y tbl)) "kn")
	       ()))
	(t nil)))

(defun knight-can-move-to (tbl)
  ;; knight current position [x,y]

  ;; add ± 2 to the x or y axys and and then ±1 to the other and check if it is out
  ;; of bounds

  ;; if not, access the table in that same position and check if the value in there is null

  ;; it shall return a list of T or NIL with size 8, which represents each move possible
  ;; for a knight clockwise (1st: +2y +1x)

  )


(defun knight-can-move ()
  ;; returns whether T or NIL if the knight has any move available

  )

(defun knight-move-to (tbl x y)
  
  (cond ((and (<= 0 x 9) (<= 0 y 9))
	 (let* ((old-value (nth x (nth y tbl)))
		(temp (gethash (reverse-string old-value) positions-map)))
	   ;; set current position to nil
	   (incf score (parse-integer old-value))				;; increment score
	   (setf (nth x (nth y tbl)) "kn")					;; set knight position
	   (setf (nth (first temp) (nth (second temp) tbl)) nil)		;; set symetric to nil
	   )))
  
  ;; update the knight current position

  ;; add the current value to its points

  ;; remove the symetric/double value

  )


#####################################################################################

;; BFS




#####################################################################################

;; DFS



#####################################################################################

;; A*


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


(defun printInformationOfBoards(boards)
  "Prints the informations about every board"
  (cond ((null boards) nil)
        ((stringp (first boards)) 
         (format t "Problem ~a:~%" (first boards))
         (printInformationOfBoards (rest boards)))
        ((or (numberp (first boards)) (equal 'random (first boards))) 
         (format t "Objective: ~a~%" (first boards))
         (printInformationOfBoards (rest boards)))
        (t
         (format t "Board:~%")
         (printBoard (first boards))
         (format t  "-----------------------------~%")
         (printInformationOfBoards (rest boards)))))

(defun printListOfBoards(boards)
  "Print the list of all the boards available"
  (cond ((null boards) nil)
        (t (printInformationOfBoards (first boards))
           (printListOfBoards (rest boards)))))


