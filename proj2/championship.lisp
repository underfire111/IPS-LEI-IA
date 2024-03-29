(defpackage :p34
  (:use :common-lisp))

(in-package :p34)

;; ### Globals ##############################################################################

(defparameter board nil
  "That variable keeps the inicial state of the game board.")
(defparameter positions-map (make-hash-table :test 'equal)
  "That hash-map contains the values and its current positions on the board.")
(defparameter start-time nil
  "That variable keeps the time that the program starts runing.")
(defparameter max-time nil
  "That variable has the time limit that the program has to finish its execution.")
(defparameter max-depth 15
  "That variable has the max depth that the alpha-beta algorithm shall look for results.")
(defparameter time-cap-percentage 99.5
  "That variable has a percentual value that represents the time limit.")

;; ### Main #################################################################################

(defun jogar (node time)
  "Main function for the tournament."
  (setf start-time (get-internal-real-time))
  (setf max-time (/ time 1000))
  (let* ((positions (set-up (first node))))
    (get-normal-node
     (alpha-beta
      (initialize-game (first positions) (second positions)
		       (first (second node)) (second (second node)))
      "player1" "player2" max-depth))))


(export 'jogar)


(defun unset-board ()
  "Returns the board, but with the value as numbers instead of strings."
  (mapcar #'(lambda (line)
              (mapcar #'(lambda (value)
                          (cond ((null value) nil)
                                ((numberp value) value)
                                (t (parse-integer value))))
		      line))
          board))


(defun get-normal-node (node)
  "Returns a node with the right structure for the tournament."
  (mapcar #'(lambda (value)
              (cond ((or (equal value "player1") (equal value "player2")))
                    (t (setf (nth (first value) (nth (second value) board)) nil))))
          (get-hash-table-keys (get-node-state node)))
  (let ((p1-position (gethash "player1" (get-node-state node)))
        (p2-position (gethash "player2" (get-node-state node))))
    (setf (nth (first p1-position) (nth (second p1-position) board)) -1)
    (setf (nth (first p2-position) (nth (second p2-position) board)) -2))
  (list (unset-board) (list (get-node-score-player-one node)
			    (get-node-score-player-two node))))


(defun set-up (board-tmp)
  "Sets the board and the positions map."
  (let ((knight1-position '()) (knight2-position '()) (tmp '()) (result '()))
    (dotimes (i 10)
      (dotimes (j 10)
        (let ((value (nth j (nth i board-tmp))))
          (cond ((equal value -1) 
                 (setf knight1-position (list j i))
                 (push nil tmp))
                ((equal value -2)
                 (setf knight2-position (list j i))
                 (push nil tmp))
                ((numberp value) (push (format-number value) tmp))
                (t (push nil tmp)))))
      (push (reverse tmp) result)
      (setf tmp '()))
    (setf board (reverse result))
    (populate-positions-map board)
    (list knight1-position knight2-position)))

;; ### Algorithm ###########################################################################

(defun alpha-beta (root player-one player-two depth)
  "Alpha-beta pruning algorithm."
  (if (not (knight-can-move (get-node-state root) player-one))
      (return-from alpha-beta root))
  (labels
      ((max-layer (parent current-player depth alpha beta)
	 (cond ((or (zerop depth)
		    (>= (time-available max-time start-time) time-cap-percentage)
		    (not (knight-can-move (get-node-state parent) current-player)))
		(list parent (evaluate parent player-one)))
	       (t (dolist (child (sort (partenogenese parent current-player)
				       #'(lambda (v1 v2) (> (evaluate v1 current-player)
							    (evaluate v2 current-player)))))
		    (let ((tmp (min-layer child player-two (1- depth) alpha beta)))
		      (setf alpha (if (>= (second alpha) (second tmp)) alpha tmp)))
		    (cond ((>= (second alpha) (second beta))
			   (return-from max-layer beta))))
		  alpha)))	 
       (min-layer (parent current-player depth alpha beta)
	 (cond ((or (zerop depth)
		    (>= (time-available max-time start-time) time-cap-percentage)
		    (not (knight-can-move (get-node-state parent) current-player)))
		(list parent (evaluate parent player-one)))
	       (t (dolist (child (sort (partenogenese parent current-player)
				       #'(lambda (v1 v2) (> (evaluate v1 current-player)
							    (evaluate v2 current-player)))))
		    (let ((tmp (max-layer child player-one (1- depth) alpha beta)))
		      (setf beta (if (<= (second beta) (second tmp)) beta tmp)))
		    (cond ((>= (second alpha) (second beta))
			   (return-from min-layer alpha))))
		  beta))))
    (let ((result (max-layer root player-one depth (list 0 most-negative-fixnum)
			     (list 0 most-positive-fixnum))))
      (get-move (first result) (get-node-depth (first result))
      		(1+ (get-node-depth root))))))

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
;; state score-player-one score-player-two parent depth
;;   1           2                3          4      5

(defun create-node (state player-one-score player-two-score &optional parent-node)
  "Creates a node/list that represents the current state of the game."
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
  "Creates a new node to succeed the parent one."
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
  "Create the all the successors node."
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


(defun populate-positions-map (table)
  "Populate the map with all all the values and its positions."
  (dotimes (i 10)
    (dotimes (j 10)
      (let* ((value (nth j (nth i table)))
	     (position (list j i)))
	(if (not (null value))
	    (setf (gethash value positions-map) position))))))


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
    (mapcar #'(lambda (value) (if (not (null value)) (push value values))) lst)
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


(defun initialize-game (player-one-position player-two-position
			player-one-score player-two-score)
  "Initializes both players position at the game board."
  (let ((player-one "player1")
	(player-two "player2")
	(root-state (make-hash-table :test 'equal)))
    (setf (gethash player-one root-state) player-one-position)
    (setf (gethash player-two root-state) player-two-position)
    (create-node root-state player-one-score player-two-score)))
