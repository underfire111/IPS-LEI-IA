;; ### Globals ################################################################################

(defparameter board nil)
(defparameter score nil)
(defparameter positions-map nil)


;; ### Inputs #################################################################################

(defun get-number ()
  "Receives an input from the user and guarantees that it is a number"
  (let ((value (read)))
    (if (numberp value) (return-from get-number value)))
  (format t "The answer must be a number!~%Please choose a number as an option: ")
  (get-number))


;; ### Loads ##################################################################################

(defun get-path(file)
  "Returns the path to theh files"
  (concatenate 'string "~/Desktop/IA/proj/" file))
  
(load (get-path "puzzle.lisp"))
(load (get-path "search.lisp"))

(defun load-boards (filename)
  "Reads the filename and loads every board in it"
  (let ((result '())
        (current_problem '())
        (current_board '()))
    (with-open-file (file filename :direction :input :if-does-not-exist :error)
      (loop for line = (read-line file nil)
            while line do (cond ((char= #\P (char line 0))
		      (setf current_problem
			    (list (subseq line (1+ (position #\Space line)) (1- (length line))))))
		     ((char= #\O (char line 0)) (push (get-objective line) current_problem))
		     ((char= #\- (char line 0))
		      (push (reverse (copy-list current_board)) current_problem)
		      (push (reverse (copy-list current_problem)) result)
		      (setf current_board '())
		      (setf current_problem '()))
		     (t (push (get-board line) current_board)))))
    (reverse result)))


(defun get-objective (line)
  "Parses the objetive to integer if it is a number, else returns random"
  (let ((objective (subseq line (position #\Space line) (length line))))
    (cond ((char= #\? (char objective 1)) 'RANDOM)
          (t (parse-integer objective)))))


(defun get-board (line)
  "Transforms the current line of the board into a list."
  (cond ((char= #\r (char line 0)) 'RANDOM)
        (t (loop for start = 0 then (1+ end)
		 for end = (or (position #\Space line :start start) (1- (length line)))
		 while (and end (< start (length line)))
		 collect (let ((value (string-trim " " (subseq line start (1+ end)))))
			   (if (equal value "NIL") nil value)))))) ;; Get the values as Strings


;; ### Outputs ################################################################################

(defun print-final-result(node)
  (cond ((null node) (format t  "This problem doesn't have a solution~%"))
	(t 
	 (format t "Objective: ~a Score: ~a~%" score (second node))
	 (format t "Result Board:~%")
	 (dotimes (i 10)
	   (dotimes (j 10)
	     (let ((coordinates (list j i))
		   (value (nth j (nth i board))))
	       (cond ((null value) (format t "-- "))
		     ((equal coordinates (gethash "kn" (first node))) (format t "Kn "))
		     ((not (null (gethash coordinates (first node)))) (format t "-- "))
		     (t (format t "~a " value)))))
	   (format t "~%")))))

(defun populate-positions-map (table)
  (dotimes (i 10)
    (dotimes (j 10)
      (let* ((value (nth j (nth i table)))
	     (position (list j i)))
	(if (not (null value))
	    (setf (gethash value positions-map) position))))))

(defun print-board (board &optional)
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
	  ((equal 'RANDOM (first board)) (format t "RANDOM~%"))
	  (t (print-row (first board))
	     (format t "~%")
	     (print-board (rest board))))))


(defun print-boards-information (boards)
  "Prints the informations about every board"
  (cond ((null boards) (format t  "-----------------------------~%"))
        ((stringp (first boards)) 
         (format t "Problem ~a:~%" (first boards))
         (print-boards-information (rest boards)))
        ((or (numberp (first boards)) (equal 'random (first boards))) 
         (format t "Objective: ~a~%" (first boards))
         (print-boards-information (rest boards)))
        (t (format t "Board:~%")
         (print-board (first boards))
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

;; TODO: Branching factor
;;       IDA
;;       RBFS
;;       Second Heuristic

(defun main ()
  (setf score (get-problem))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (format t "-----------------------------~%Objective:~a~%Board~%" score)
  (print-board board)
  (format t "-----------------------------~%BFS~%")
  (print-final-result (bfs))
  (format t "-----------------------------~%DFS~%")
  (print-final-result (dfs))
  (format t "-----------------------------~%A*~%Heuristic: Percentual Distance~%")
  (print-final-result (a* #'percentual-distance
			  #'(lambda(n1 n2)
			      (> (fifth n1) (fifth n2)))))
  (format t "Heuristic: Enunciation~%")
  (print-final-result (a* #'enunciation-heuristic
			  #'(lambda(n1 n2)
			      (< (fifth n1) (fifth n2))))))

(defun test()
  (setf score (get-problem))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (format t "-----------------------------~%A*~%Heuristic: Percentual Distance~%")
  (print-final-result (a* #'percentual-distance
			  #'(lambda(n1 n2)
			      "Auxialiar method to sort the value in the open nodes list of A* algorithm. Checks if node1 as a  greater cost than node2"
			      (> (fifth n1) (fifth n2)))))
  (print ""))

(defun test1()
  (setf score (get-problem))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (format t "-----------------------------~%DFS~%")
  (print-final-result (dfs))
  (print ""))

(defun test2()
  (setf score (get-problem))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (format t "-----------------------------~%BFS~%")
  (print-final-result (bfs))
  (print ""))

(defun test3()
  (setf score (get-problem))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (format t "-----------------------------~%Objective:~a~%Board~%" score)
  (print-board board)
  (format t "-----------------------------~%A*~%Heuristic: Percentual Distance~%")
  (print-final-result (a* #'percentual-distance
			  #'(lambda(n1 n2)
			      (> (fifth n1) (fifth n2)))))
  (format t "Heuristic: Enunciation~%")
  (print-final-result (a* #'enunciation-heuristic
			  #'(lambda(n1 n2)
			      (< (fifth n1) (fifth n2))))))
