;; ### Loads ################################################################################

(defun get-folder-path (file)
  "Returns the path to theh files"
  (concatenate 'string "~/Desktop/IA/proj2/" file))

(load (get-folder-path "game.lisp"))
(load (get-folder-path "algorithm.lisp"))

;; ### Globals ##############################################################################

(defparameter board nil)
(defparameter score nil)
(defparameter positions-map nil)

;; ### Inputs ###############################################################################

(defun get-number()
  "Ask for input until get a number"
  (format t "Enter a number > ")
  (let ((value (read)))
    (if (numberp value) (return-from get-number value)))
  (sleep 0.01)
  (format t "Invalid input! Please enter a number.")
  (get-number))

;; ### Output ###############################################################################

(defun print-board (board &optional curr-state)
  "Prints the board"
  (cond ((null board) nil)
        (t (mapcar #'(lambda(line)
                       "Prints the line of the board and a new line at the end"
                       (mapcar #'(lambda(value)
                                   "Prints the value in the line"
                                   (let ((coordinates (gethash value positions-map)))
                                   (cond ((or (null value) (and curr-state )) (format t "-- "))
                                         ((and curr-state)
                                          (cond ((gethash coordinates curr-state) (format t "-- "))
                                                ((equal coordinates (gethash "p1" curr-state)) (format t "P1 "))
                                                ((equal coordinates (gethash "p2" curr-state)) (format t "P2 "))
                                                (t (format t "~a " value))))
                                         (t (format t "~a " value)))))
                               line)
                       (format t "~%"))
                   board))))

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

;; ### Main ###############################################################################

(defun main()
  (setf board (mount-board (shuffle-positions (list-positions))))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (format t "Board:~%")
  (print-board board)
  (format t "Do you want a score in the game? (Yes or No) ")
  (let ((answer (read)))
    (cond ((or (equal answer 'yes) (equal answer 'Yes))
           (format t "How much? ")
           (setf score (get-number)))
          (t (setf score 4000)))) ;; 4000 impossible value to obtain due to the rule of the removed values. 
  (format t "--------------------------~%")
  (print "Success"))

(defun jogar(state time)
  "Returns the board with the next movement done"
)