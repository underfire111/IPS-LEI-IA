;; ### Loads ################################################################################

(defun get-folder-path (file)
  "Returns the path to theh files"
  (concatenate 'string "~/Desktop/IA/proj2/" file))

(load (get-folder-path "game.lisp"))
(load (get-folder-path "algorithm.lisp"))

;; ### Globals ##############################################################################

(defparameter board nil)
(defparameter positions-map nil)
(defparameter max-time nil)

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


(defun main ()
  (setf board (mount-board (shuffle-list-positions (list-positions))))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (format t "Enter the time limit in the range of [1,5] seconds: ~%")
  (let ((number (get-number)))
    (setf max-time (if (or (< number 1) (> number 5)) 5 number)))
  (let ((node (initialize-game)))
    (format t "Board:~%")
    (print-board (get-node-state node))
    (format t "~%Player1 score: ~a" (get-node-score-player-one node))
    (format t "~%Player2 score: ~a~%" (get-node-score-player-two node))
    (let* ((result (game-loop node "player1" "player2"))
           (score-player1 (get-node-score-player-one result))
           (score-player2 (get-node-score-player-two result)))
      (cond ((= score-player1 score-player2)
	     (format t "~%Draw"))
            ((> score-player1 score-player2)
	     (format t "~%Winner: Player1~%Score: ~a" score-player1))
            (t (format t "~%Winner: Player2~%Score: ~a" score-player2))))))


(defun main (&key (time-limit))
  (setf board (mount-board (shuffle-list-positions (list-positions))))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (setf max-time (if (or (< time-limit 1) (> time-limit 5)) 5 time-limit))
  (let ((node (initialize-game)))
    (let* ((result (game-loop node "player1" "player2"))
           (score-player1 (get-node-score-player-one result))
           (score-player2 (get-node-score-player-two result)))
      (cond ((= score-player1 score-player2) 0)
            ((> score-player1 score-player2) 1)
            (t 2)))))


(defun count-main-frequencies ()
  (let ((count-0 0)
        (count-1 0)
        (count-2 0))
    (dotimes (i 1)
      (let ((result (main :time-limit 1)))
        (case result
          (0 (incf count-0))
          (1 (incf count-1))
          (2 (incf count-2)))))
    (format t "Frequency of 0: ~d~%" count-0)
    (format t "Frequency of 1: ~d~%" count-1)
    (format t "Frequency of 2: ~d~%" count-2)))


(defun game-loop (node player1 player2)
  (if (not (knight-can-move (get-node-state node) player1))
      (if (not (knight-can-move (get-node-state node) player2))
      (return-from game-loop node)))
  (format t "~%~a Move: ~%" player1)
  (if (equal player1 "player1")
      (setf node (alpha-beta-a node player1 player2  :depth 3))
      (setf node (alpha-beta-b node player1 player2  :depth 3)))
  (print-board (get-node-state node))
  (if (equal player1 "player1")
      (format t "~%~a score: ~a~%" player1 (get-node-score-player-one node))
      (format t "~%~a score: ~a~%" player1 (get-node-score-player-two node)))
  (game-loop node player2 player1))


(defun game-loop (node player1 player2)
  (if (not (knight-can-move (get-node-state node) player1))
      (if (not (knight-can-move (get-node-state node) player2))
      (return-from game-loop node)))
  (if (equal player1 "player1")
      (setf node (alpha-beta-a node player1 player2  :depth 8))
      (setf node (alpha-beta-b node player1 player2  :depth 8)))
  (game-loop node player2 player1))

