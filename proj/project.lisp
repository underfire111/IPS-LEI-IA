;; ### Globals ################################################################################

(defparameter board nil)
(defparameter scoren nil)
(defparameter positions-map nil)
;; ### Inputs #################################################################################

(defun get-number ()
  "Receives an input from the user and guarantees that it is a number"
    (let ((value (read)))
      (if (numberp value) (return-from get-number value)))
  (print "The answer must be a number!
Please choose a number as an option:") 
  (get-number))


;; ### Loads ##################################################################################

(defun get-path(file)
  "Returns the path to the files"
  (concatenate 'string "~/Desktop/IA/proj/" file))
  
(load (get-path "puzzle.lisp"))

(defun load-boards (filename)
  "Reads the filename and loads every board in it"
  (let ((result '())
        (current_problem '())
        (current_board '()))
    (with-open-file (file filename :direction :input :if-does-not-exist :error)
      (loop for line = (read-line file nil)
            while line
            do
              (cond ((char= #\P (char line 0))
                     (setf current_problem (list (subseq line (1+ (position #\Space line))
							 (1- (length line)))))) ;; Gets the problem's name
                    ((char= #\O (char line 0)) ;; Gets its objective
                     (push (get-objective line) current_problem))
                    ((char= #\- (char line 0)) ;; Separator between problems
                     (push (reverse (copy-list current_board)) current_problem) ;; stores current problem
                     (push (reverse (copy-list current_problem)) result)
                     (setf current_board '()) ;; Resets variables
                     (setf current_problem '()))
                    (t (push (get-board line) current_board))))) ;; Gets the current board
    (reverse result)))


(defun get-objective (line)
  "Parses the objetive to integer if it is a number, else returns random"
  (let ((objective (subseq line (position #\Space line) (length line))))
    (cond ((char= #\? (char objective 1)) 'random)
          (t (parse-integer objective)))))


(defun get-board (line)
  "Transforms the current line of the board into a list."
  (cond ((char= #\r (char line 0)) 'random)
        (t (loop for start = 0 then (1+ end)
		 for end = (or (position #\Space line :start start) (1- (length line)))
		 while (and end (< start (length line)))
		 collect (let ((value (string-trim " " (subseq line start (1+ end)))))
			   (if (equal value "NIL") nil value)))))) ;; Get the values as Strings


;; ### Outputs ################################################################################

(defun print-board (board)
  "Prints the board"
  (labels ((print-row(line)
	     "Prints a string"
	     (cond ((null line) nil)
		   ((null (first line))
		    (format t "-- ")
		    (print-row (rest line)))
		   (t (format t "~a " (first line))
		      (print-row (rest line))))))
    (cond ((null board) nil)
	  ((equal 'random (first board)) (format t "Random~%"))
	  (t (print-row (first board))
	     (format t "~%")
	     (print-board (rest board))))))


(defun print-boards-information (boards)
  "Prints the informations about every board"
  (cond ((null boards) nil)
        ((stringp (first boards)) 
         (format t "Problem ~a:~%" (first boards))
         (print-boards-information (rest boards)))
        ((or (numberp (first boards)) (equal 'random (first boards))) 
         (format t "Objective: ~a~%" (first boards))
         (print-boards-information (rest boards)))
        (t (format t "Board:~%")
         (print-board (first boards))
         (format t  "-----------------------------~%")
         (print-boards-information (rest boards)))))


(defun print-boards-list (boards)
  "Print the list of all the boards available"
  (cond ((null boards) nil)
        (t (print-boards-information (first boards))
           (print-boards-list (rest boards)))))


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


;; ### Main ###################################################################################

(defun get-problem ()
  "Returns the goal to the selected problem"
  (let* ((file (get-path "boards.dat" ))
	 (boards (load-boards file)))
    (print-boards-list boards)
    (format t "Choose the problem (the choice must be a number): ")
    (let* ((option (get-number))
	   (temp nil))
      (cond ((not (or (< option 1) (>= option (length boards))))
	     (setf temp (nth (1- option) boards))
	     (if (stringp (third temp)) (setf board (mount-board
						     (shuffle-positions (list-positions))))
		 (setf board (third temp)))
	     (if (stringp (second temp)) (random 3245) (second temp)))
	    (t (setf board (mount-board (shuffle-positions (list-positions))))
	       (random 3245))))))


(defun initialize-state ()
  "Initialize the state of the problem by getting the start position of the knight"
  (populate-positions-map board)
  (labels ((first-knight-position(first-row)
               "Gets the first position of the knight"
               (let ((position (get-number)))
                 (if (and 
                      (<= position 10) 
                      (>= position 1) 
                      (not (equal (nth (1- position) first-row) "NIL ")))
                     (return-from  first-knight-position (1- position))))
               (print "Please choose a number between [1;10] that isn't '--'")
               (first-knight-position first-row)))
    (print "Choose the position(number in [1;10] of the first row) to start the knight in: ")
    (knight-start-position 0 (first-knight-position (first board)))))


(defun main ()
  (let* ((goal (get-problem)))
    (setf positions-map (make-hash-table :test 'equal))
    (populate-positions-map board)))
        

