;; ### Globals ##############################################################################

(defparameter board nil)
(defparameter score nil)
(defparameter positions-map nil)


;; ### Inputs ###############################################################################

(defun get-number ()
  "Ask for input until get a number"
  (format t "Enter a number > ")
  (let ((value (read)))
    (if (numberp value) (return-from get-number value)))
  (sleep 0.01)
  (format t "Invalid input! Please enter a number.")
  (get-number))


;; ### Loads ################################################################################

(defun get-folder-path (file)
  "Returns the path to theh files"
  (concatenate 'string "~/Desktop/IA/proj/" file))


(load (get-folder-path "puzzle.lisp"))
(load (get-folder-path "search.lisp"))

 
(defun load-boards (filename)
  "Reads the filename and loads every board in it"
  (let ((result '())
        (current_problem '())
        (current_board '()))
    (with-open-file (file filename :direction :input :if-does-not-exist :error)
      (loop for line = (read-line file nil)
            while line do
	      (cond ((char= #\P (char line 0))
		     (setf current_problem
			   (list (subseq line (1+ (position #\Space line))
					 (1- (length line))))))
		    ((char= #\O (char line 0))
		     (push (get-objective line) current_problem))
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
			   (if (equal value "NIL") nil value))))))


;; ### Output ###############################################################################

(defun write-solution-to-file (node label)
  "Writes the solution to a file called solution"
  (labels
      ((aux-function (node fd)
	 "Write at the file descriptor all the states in the solutions."
	 (cond ((null (get-node-parent node))
		(print-current-state node fd))
	       (t (aux-function (get-node-parent node) fd)
		  (print-current-state node fd)))
	 (format fd "~%")))
    (with-open-file
	(fd (get-folder-path "solutions.dat")
	      :direction :output
	      :if-exists :append
	      :if-does-not-exist :create)
      (format fd "#############################~%")
      (format fd "### ~a~%" label)
      (format fd "### ~a~%" score)
      (format fd "#############################~%~%")
      (aux-function node fd)
      )))


(defun print-current-state (node &optional (fd t))
  (cond ((null node) (format fd "This problem doesn't have a solution~%"))
	(t 
	 (format fd "Score: ~a~%" (get-node-score node))
	 (format fd "Board: ~%")
	 (dotimes (y 10)
	   (dotimes (x 10)
	     (let ((coordinates (list x y))
		   (value (nth x (nth y board))))
	       (cond ((null value) (format fd "-- "))
		     ((equal
		       coordinates
		       (gethash "kn" (get-node-state node)))
		      (format fd "Kn "))
		     ((gethash coordinates (get-node-state node))
		      (format fd "-- "))
		     (t (format fd "~a " value)))))
	   (format fd "~%")))))


(defun print-board (board)
  "Prints the board"
  (labels
      ((print-row(line)
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


(defun print-hash-table (tbl)
  "Print a map"
  (maphash #'(lambda (key value)
	       (format t "Key: ~a, Position: ~a~%" key value))
	   tbl))


(defun print-hash-table-sorted (tbl)
  "Print a map with its keys sorted"
  (let ((keys (get-hash-table-keys tbl)))
    (dolist (key (sort keys 'string<))
      (format t "Key: ~a, Position: ~a~%" key (gethash key tbl)))))


;; ### Main ################################################################################

;; TODO: Branching factor
;;       IDA
;;       RBFS
;;       Second Heuristic

(defun init ()
  (setf score (get-problem))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  )


(defun main ()
  (init)
  ; (write-solution-to-file (breadth-first-search) "BFS")
  (write-solution-to-file (depth-first-search) "DFS")
  (write-solution-to-file (a* #'calc-percentual-distance) "A* (Percentual Distance)")
  ; (write-solution-to-file (a* #'calc-movements-left) "A* (Movements Left)")
  (write-solution-to-file (ida* #'calc-percentual-distance) "IDA* (Percentual Distance)")
  ; (write-solution-to-file (ida* #'calc-movements-left) "IDA* (Movements Left)")
  (write-solution-to-file (sma* #'calc-percentual-distance 2) "SMA* (Percentual Distance)")
  ; (write-solution-to-file (sma* #'calc-movements-left 10) "SMA* (Movements Left)")
  )


(defun test-dfs ()
  (init)
  (format t "-----------------------------~%DFS~%")
  (print-current-state (depth-first-search)))


(defun test-bfs ()
  (init)
  (format t "-----------------------------~%BFS~%")
  (print-current-state (breadth-first-search)))


(defun test-a* ()
  (init)
  (format t "-----------------------------~%A*~%")
  (print-current-state (a* #'calc-percentual-distance))
  (print-current-state (a* #'calc-movements-left)))


(defun test-ida* ()
  (init)
  (format t "-----------------------------~%IDA*~%")
  (print-current-state (ida* #'calc-percentual-distance))
  (print-current-state (ida* #'calc-movements-left)))


(defun test-sma* ()
  (init)
  (format t "-----------------------------~%SMA*~%")
  (print-current-state (sma* #'calc-percentual-distance 3))
  (print-current-state (sma* #'calc-movements-left 3)))
