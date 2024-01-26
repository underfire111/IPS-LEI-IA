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
(defparameter max-depth nil)

;; ### Inputs ###############################################################################

(defun get-number()
  "Ask for input until get a number"
  (format t "Enter a number > ")
  (let ((value (read)))
    (sleep 0.01)
    (format t "~%")
    (if (numberp value) (return-from get-number value)))
  (format t "Invalid input! Please enter a number.")
  (get-number))

(defun get-valid-number (cap)
  (let ((number (get-number)))
    (if (or (<= number 0) (> number cap))
	(get-valid-number cap)
	(return-from get-valid-number number))))

;; ### Output ###############################################################################

(defun write-solution-to-file (algorithm-result)
  "Writes the current state and some information of the alghorithm to a file called log.dat."
  (with-open-file (fd (get-folder-path "log.dat")
                      :direction :output
                      :if-exists :append
                      :if-does-not-exist :create)
    (format fd "################################~%")
    (format fd "### Number of Nodes analised: ~a~%" (third algorithm-result))
    (format fd "### Number of Min cuts: ~a~%" (fourth algorithm-result))
    (format fd "### Number of Max cuts: ~a~%" (fifth algorithm-result))
    (format fd "### Time(s): ~a~%" (sixth algorithm-result))
    (format fd "################################~%~%")
    (print-board (get-node-state (first algorithm-result)) fd)
    (format fd "################################~%")
    (format fd "################################~%~%")))


(defun print-board (&optional curr-state (fd t))
  "Prints the board"
  (format fd "   | A  B  C  D  E  F  G  H  I  J  ~%")
  (format fd "---|-------------------------------~%")
  (let ((line-number '("01 | " "02 | " "03 | " "04 | " "05 | " "06 | " "07 | " "08 | " "09 | " "10 | ")))
    (cond ((null board) nil)
          (t (mapcar
              #'(lambda (line)
                  "Prints the line of the board and a new line at the end"
                  (format fd (pop line-number))
                  (mapcar
                   #'(lambda (value)
                       "Prints the value in the line"
                       (let ((coordinates (gethash value positions-map)))
                         (cond ((or (null value)) (format fd "-- "))
                               (curr-state
                                (cond ((equal coordinates (gethash "player1" curr-state))
                                       (format fd "▣1 "))
                                      ((equal coordinates (gethash "player2" curr-state))
                                       (format fd "▣2 "))
                                      ((gethash coordinates curr-state) (format fd "-- "))
                                      (t (format fd "~a " value))))
                               (t (format fd "~a " value)))))
                   line)
                  (format fd "~%"))
	      board)))))


(defun print-hash-table (tbl)
  "Print a map"
  (maphash #'(lambda (key value) (format t "Key: ~a, Position: ~a~%" key value)) tbl))


(defun print-hash-table-sorted (tbl)
  "Print a map with its keys sorted"
  (let ((keys (get-hash-table-keys tbl)))
    (dolist (key (sort keys 'string<))
      (format t "Key: ~a, Position: ~a~%" key (gethash key tbl)))))


(defun print-available-moves (lst)
  (format t "~%Available moves:~%~%")
  (loop for (first second) in lst
        for index from 1
        do (format t "~a) ~c~a~%" index (code-char (+ 65 first)) (1+ second))))


;; ### Main ###############################################################################

(defun main (&key (time-limit 5) (depth 8))
  (setf board (mount-board (shuffle-list-positions (list-positions))))
  (setf positions-map (make-hash-table :test 'equal))
  (populate-positions-map board)
  (setf max-time (if (<= time-limit 0) 5 time-limit))
  (setf max-depth (if (or (< depth 3) (> depth 30)) 9 depth))
  (format t "GAME MODE:~%~%1) Player vs AI~%2) AI vs AI~%~%")
  (let ((number (get-number)))
    (cond ((= number 1) (set-up-game-environment #'game-loop-player-vs-ai))
	  ((= number 2) (set-up-game-environment #'game-loop-ai-vs-ai))
	  (t (format t "Invalid game mode!")))))


(defun set-up-game-environment (game-loop)
  (let ((node (initialize-game)))
    (format t "Board:~%")
    (print-board (get-node-state node))
    (format t "~%Player1 score: ~a" (get-node-score-player-one node))
    (format t "~%Player2 score: ~a~%" (get-node-score-player-two node))
    (let* ((result (funcall game-loop node "player1" "player2"))
           (score-player1 (get-node-score-player-one result))
           (score-player2 (get-node-score-player-two result)))
      (cond ((= score-player1 score-player2) (format t "~%Draw"))
            ((> score-player1 score-player2)
	     (format t "~%Winner: Player1~%Score: ~a" score-player1))
            (t (format t "~%Winner: Player2~%Score: ~a" score-player2))))))


(defun game-loop-player-vs-ai (node player1 player2)
  (if (not (knight-can-move (get-node-state node) player1))
      (if (not (knight-can-move (get-node-state node) player2))
	  (return-from game-loop-player-vs-ai node)))
  (format t "~%############## PLAYER ##############~%~%")
  (let* ((available-moves (knight-can-move-to (get-node-state node) player1))
	 (number-of-moves (length available-moves)))
    (cond ((> number-of-moves 0)
	   (print-available-moves available-moves)
	   (setf
	    node (create-next-node
		  (knight-move-to
		   (get-node-state node)
		   (nth (1- (get-valid-number number-of-moves)) available-moves)
		   player1)
		  node player1)))
	  (t (setf node (create-next-node (get-node-state node) node player1)))))
  (print-board (get-node-state node))
  (format t "~%Player score: ~a~%" (get-node-score-player-one node))
  (format t "AI score: ~a~%" (get-node-score-player-two node))
  (format t "~%################ AI ################~%~%")
  (setf node (alpha-beta node player2 player1 :depth max-depth))
  (print-board (get-node-state node))
  (format t "~%Player score: ~a~%" (get-node-score-player-one node))
  (format t "AI score: ~a~%" (get-node-score-player-two node))
  
  (game-loop-player-vs-ai node player1 player2))


(defun game-loop-ai-vs-ai (node player1 player2)
  (if (not (knight-can-move (get-node-state node) player1))
      (if (not (knight-can-move (get-node-state node) player2))
	  (return-from game-loop-ai-vs-ai node)))
  (format t "~%~a Move: ~%" player1)
  (if (equal player1 "player1")
      (setf node (alpha-beta node player1 player2 max-depth))
      (setf node (alpha-beta node player1 player2 max-depth)))
  (print-board (get-node-state node))
  (if (equal player1 "player1")
      (format t "~%~a score: ~a~%" player1 (get-node-score-player-one node))
      (format t "~%~a score: ~a~%" player1 (get-node-score-player-two node)))
  (game-loop-ai-vs-ai node player2 player1))


