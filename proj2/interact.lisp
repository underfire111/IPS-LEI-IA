;; ### Loads ################################################################################

(defun get-folder-path (file)
  "Returns the path to theh files"
  (concatenate 'string "~/Desktop/IA/proj2/" file))

(load (get-folder-path "game.lisp"))
(load (get-folder-path "algorithm.lisp"))

;; ### Globals ##############################################################################

(defparameter board nil)
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

(defun print-board (&optional curr-state)
  "Prints the board"
  (cond ((null board) nil)
	(t (mapcar
	    #'(lambda (line)
		"Prints the line of the board and a new line at the end"
		(mapcar
		 #'(lambda (value)
		     "Prints the value in the line"
		     (let ((coordinates (gethash value positions-map)))
		       (cond ((or (null value)) (format t "-- "))
			     (curr-state
			      (cond ((equal coordinates (gethash "player1" curr-state))
				     (format t "♞1 "))
				    ((equal coordinates (gethash "player2" curr-state))
				     (format t "♞2 "))
				    ((gethash coordinates curr-state) (format t "-- "))
				    (t (format t "~a " value))))
			     (t (format t "~a " value)))))
		 line)
		(format t "~%"))
	    board))))

(defun print-hash-table (tbl)
  "Print a map"
  (maphash #'(lambda (key value) (format t "Key: ~a, Position: ~a~%" key value)) tbl))


(defun print-hash-table-sorted (tbl)
  "Print a map with its keys sorted"
  (let ((keys (get-hash-table-keys tbl)))
    (dolist (key (sort keys 'string<))
      (format t "Key: ~a, Position: ~a~%" key (gethash key tbl)))))

;; ### Main ###############################################################################


(defun player-input (current-state)
  (let ((possible-moves (knight-can-move-to current-state current-player)))))


(defun main()
  (setf board (mount-board (shuffle-list-positions (list-positions))))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (let ((node (initialize-game)))
    (format t "Board:~%")
    (print-board (get-node-state node))
    (print-hash-table (get-node-state node))
    (game-loop node "player1" "player2")
    )
  (print "Success"))

(defun game-loop (node player1 player2)
  (if (not (knight-can-move (get-node-state node) player1))
      (if (not (knight-can-move (get-node-state (get-node-parent node)) player1))
	  (return-from game-loop player1)))
  (format t "~%~a Move: ~%" player1)
  (setf node (alpha-beta node player1 player2 :depth 10))
  (if (equal player1 "player1")
      (format t "~%~a score: ~a~%" player1 (get-node-score-player-one node))
      (format t "~%~a score: ~a~%" player1 (get-node-score-player-two node)))
  (print-board (get-node-state node))
 ;; (print-hash-table (get-node-state node))
  (game-loop node player2 player1))

(defun jogar(state time)
  "Returns the board with the next movement done"
)
