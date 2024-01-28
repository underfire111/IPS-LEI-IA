(in-package :p34)


;; ### Globals ##############################################################################

(defparameter board nil)
(defparameter positions-map nil)
(defparameter max-time nil)
(defparameter max-depth 15)
(defparameter time-cap-percentage 80)

;; ### Main Function ########################################################################

(defun unset-board()
  "Returns the board, but with the value as numbers instead of strings."
  (mapcar #'(lambda(line)
              (mapcar #'(lambda(value)
                          (cond ((null value) nil)
                                ((numberp value) value)
                                (t (parse-integer value)))) line))
          board))

(defun get-normal-node(node)
  "Returns a node with the structure of the championship."
  (mapcar #'(lambda(value)
              (cond ((or (equal value "player1") (equal value "player2")))
                    (t (setf (nth (first value) (nth (second value) board)) nil))))
          (get-hash-table-keys (get-node-state node)))
  (let ((p1-position (gethash "player1" (get-node-state node)))
        (p2-position (gethash "player2" (get-node-state node))))
    (setf (nth (first p1-position) (nth (second p1-position) board)) -1)
    (setf (nth (first p2-position) (nth (second p2-position) board)) -2))
  (list (unset-board) (list (get-node-score-player-one node) (get-node-score-player-two node))))
  

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
                                       (format fd "P1 "))
                                      ((equal coordinates (gethash "player2" curr-state))
                                       (format fd "P2 "))
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

(defun jogar(no time)
  "Main function for the championship"
  (setf max-time time)
  (let* ((positions (set-up (first no) (first (second no)) (second (second no)))))
    (get-normal-node (alpha-beta (initialize-game (first positions) (second positions)) "player1" "player2" 9))))
(export 'jogar)

(defun set-up(board-tmp score-player1 score-player2)
  "Sets the board and the positions map."
  (let ((knight1-position '())
        (knight2-position '())
        (tmp '())
        (result '()))
    (dotimes (i 10)
      (dotimes (j 10)
        (let ((value (nth j (nth i board-tmp))))
          (cond ((equal value -1) 
                 (setf knight1-position (list j i))
                 (push (format-number score-player1) tmp))
                ((equal value -2)
                 (setf knight2-position (list j i))
                 (push (format-number score-player2) tmp))
                ((numberp value) (push (format-number value) tmp))
                (t (push nil tmp)))))
      (push (reverse tmp) result)
      (setf tmp '()))
    (setf board (reverse result))
    (setf positions-map (make-hash-table :test 'equal))
    (populate-positions-map board)
    (list knight1-position knight2-position)))

;; ### Algorithm ############################################################################

(defun alpha-beta (root player1 player2 depth)
  "Alpha Beta"
  (if (not (knight-can-move (get-node-state root) player1)) (return-from alpha-beta root))
  (let ((start-time (get-internal-real-time)))
    (labels
	((max-layer (parent current-player depth alpha beta)
	   "Max layer in the algorithm."
	   (cond
	     ((or (zerop depth)
		  (>= (time-available max-time start-time) time-cap-percentage)
		  (not (knight-can-move (get-node-state parent) current-player)))
	      (list parent (evaluate parent player1)))
	     (t (dolist (child (sort (partenogenese parent current-player)
				     #'(lambda (v1 v2) (> (evaluate v1 current-player)
							  (evaluate v2 current-player)))))
		  (let ((tmp (min-layer child player2 (1- depth) alpha beta)))
		    (setf alpha (if (>= (second alpha) (second tmp)) alpha tmp)))
		  (cond ((>= (second alpha) (second beta))
			 (return-from max-layer beta))))
		alpha)))	 
	 (min-layer (parent current-player depth alpha beta)
	   "Max layer in the algorithm."
	   (cond
	     ((or (zerop depth)
		  (>= (time-available max-time start-time) time-cap-percentage)
		  (not (knight-can-move (get-node-state parent) current-player)))
	      (list parent (evaluate parent player1)))
	     (t (dolist (child (sort (partenogenese parent current-player)
				     #'(lambda (v1 v2) (> (evaluate v1 current-player)
							  (evaluate v2 current-player)))))
		  (let ((tmp (max-layer child player1 (1- depth) alpha beta)))
		    (setf beta (if (<= (second beta) (second tmp)) beta tmp)))
		  (cond ((>= (second alpha) (second beta))
		       (return-from min-layer alpha))))
		beta))))
      (let ((result (max-layer root player1 depth (list 0 most-negative-fixnum)
			       (list 0 most-positive-fixnum))))
        (get-move (first result) (get-node-depth (first result))
		  (1+ (get-node-depth root)))))))

;; ### Knight ##############################################################################

(defun knight-can-move-to (current-state current-player)
  "Check for all possible moves."
  (cond ((not (null (gethash current-player current-state)))
	 (let ((x (first (gethash current-player current-state)))
	       (y (second (gethash current-player current-state)))
	       (moves '()))
	   (dolist (offset '((1 . 2) (2 . 1) (2 . -1) (1 . -2)
			     (-1 . -2) (-2 . -1) (-2 . 1) (-1 . 2)))
	     (let ((new-x (+ x (car offset))) (new-y (+ y (cdr offset))))
	       (cond ((and (>= new-x 0) (<= new-x 9) (>= new-y 0) (<= new-y 9)
			   (not (gethash (list new-x new-y) current-state))
			   (nth new-x (nth new-y board)))
		      (push (list new-x new-y) moves))
		     (t (push nil moves)))))
	   (remove-nil (reverse moves))))))


(defun knight-can-move (current-state current-player)
  "Check if the knight still have any move available."
  (let ((next (knight-can-move-to current-state current-player)))
    (not (notany #'identity next))))


(defun knight-move-to (current-state coordinates current-player)
  "If possible, move the knight to the given location"
  (cond (current-state
	 (cond (coordinates
		(let ((value (nth (first coordinates) (nth (second coordinates) board))))
		  (setf (gethash current-player current-state) coordinates)
		  (setf (gethash coordinates current-state) "#MOV")
		  (remove-symmetric-or-double current-state value)))
	       (t (error "Invalid move!")))
	 current-state)
	(t (error "Invalid state!"))))


;; ### Node ################################################################################

;; node e uma lista com:
;; state score-player1 score-player2 parent
;;   1        2             3           4

(defun create-node (state player-one-score player-two-score &optional parent-node)
  (if (and state (numberp player-one-score) (numberp player-two-score))
      (list state player-one-score player-two-score
	    (if parent-node parent-node nil)
	    (if parent-node (1+ (get-node-depth parent-node)) 0))))


(defun get-node-depth (node)
  (fifth node))


(defun get-node-parent (node)
  (fourth node))


(defun get-node-state (node)
  (first node))


(defun get-node-score-player-one (node)
  (second node))


(defun get-node-score-player-two (node)
  (third node))


(defun create-next-node (current-state parent-node current-player)
  "Creates a new node."
  (cond ((and current-state parent-node
	      (or (equal current-player "player1") (equal current-player "player2")))
	 (let* ((player-coordinates (gethash current-player current-state))
		(score (get-board-value player-coordinates)))
	   (cond ((equal current-player "player1")
		  (create-node current-state
			       (+ score (get-node-score-player-one parent-node))
			       (get-node-score-player-two parent-node)
			       parent-node))
		 ((equal current-player "player2")
		  (create-node current-state
			       (get-node-score-player-one parent-node)
			       (+ score (get-node-score-player-two parent-node))
			       parent-node))
		 (t nil))))
	(t nil)))


(defun partenogenese (current-node current-player)
  "Create the node successors."
  (mapcar
   #'(lambda (coordinates)
       "Creates a node moving the knight to the coordinates."
       (create-next-node
	(knight-move-to
	 (clone-hash-table (get-node-state current-node))
	 coordinates
	 current-player)
	current-node
	current-player))
   (knight-can-move-to (get-node-state current-node) current-player)))

;; ### Utils ###############################################################################


(defun get-board-value (coordinates)
  "Returns the value at the position xy from the board."
  (parse-integer (nth (first coordinates) (nth (second coordinates) board))))


(defun get-doubles ()
  "Return a list with all doubles possible."
  '("99" "88" "77" "66" "55" "44" "33" "22" "11" "00"))

(defun get-hash-table-keys (tbl)
  "Returns a list with only the values in the board as strings."
  (let ((keys '()))
    (maphash #'(lambda (k v) (declare (ignore v)) (push k keys)) tbl)
    keys))


(defun clone-hash-table (tbl)
  "Creates a shallow copy of a hash table."
  (let ((new-table (make-hash-table
		    :test (hash-table-test tbl)
		    :size (hash-table-size tbl))))
    (maphash #'(lambda (key value) (setf (gethash key new-table) value)) tbl)
    new-table))


(defun remove-nil (lst)
  "Removes all the nil values of the list."
  (let ((values '()))
    (mapcar #'(lambda(value) (if (not (null value)) (push value values))) lst)
    (reverse values)))


(defun remove-symmetric-or-double (current-state value)
  "Removes doubles or symetrics."
  (let ((temp (coerce (reverse (coerce value 'list)) 'string)))
    (cond ((not (equal temp value))
	   (if (gethash temp positions-map)
	       (setf (gethash (gethash temp positions-map) current-state) "#SYM")))
	  (t (let ((doubles
		     (remove-nil
		      (mapcar #'
		       (lambda (key)
			 (let ((double-coordinate (gethash key positions-map)))
			   (if (and double-coordinate
				    (null (gethash double-coordinate current-state)))
			       double-coordinate))) 
		       (get-doubles)))))
	       (if (null (gethash (first doubles) current-state))
		   (setf (gethash (first doubles) current-state) "#DBL")))))))

(defun evaluate (current-node current-player)
  "Returns the difference of the scores between the players."
  (let ((score-player-one (get-node-score-player-one current-node))
        (score-player-two (get-node-score-player-two current-node)))
    (cond ((equal current-player "player1") (- score-player-one score-player-two))
	  (t (- score-player-two score-player-one)))))

(defun time-available(time-limit start)
  "Returns the percentage of time passed."
  (* (/ (- (get-internal-real-time) start) internal-time-units-per-second time-limit) 100))


(defun get-move (current-node current-depth root-depth)
  "Gets the node above height layers of node" 
  (cond ((equal root-depth current-depth) current-node)
        (t (get-move (get-node-parent current-node) (1- current-depth) root-depth))))

(defun format-number(number)
  "Transforms the number into a string."
  (format nil "~2,'0D" number))

(defun populate-positions-map (table)
  "Populate the map with all all the values and its positions."
  (dotimes (i 10)
    (dotimes (j 10)
      (let* ((value (nth j (nth i table)))
	     (position (list j i)))
	(if (not (null value))
	    (setf (gethash value positions-map) position))))))

(defun initialize-game (player-one-position player-two-position)
  "Initializes both players position at the game board."
  (let ((player-one "player1")
	(player-two "player2")
	(root-state (make-hash-table :test 'equal)))
    (setf (gethash player-one root-state) nil)
    (setf (gethash player-two root-state) nil)
    (let* ((root (create-node root-state 0 0))
	   (first-node
	     (create-next-node
	      (knight-move-to
	       (get-node-state root)
	       player-one-position
	       player-one)
	      root
	      player-one))
	   (second-node
	     (create-next-node
	      (knight-move-to
	       (get-node-state first-node)
	       player-two-position
	       player-two)
	      first-node
	      player-two)))
      second-node)))