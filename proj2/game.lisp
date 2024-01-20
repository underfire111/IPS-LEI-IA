;; positions-map	| key: "value"  value: (z y)
;; state-map		| key: (x y)    value: "#CODE" 

;; ### Board ################################################################################

(defun list-positions ()
  "List all values possible."
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


(defun shuffle-list-positions (lst)
  "Shiffle a list."
  (labels ((shuffle-helper (lst n)
	     (cond ((<= n 1) lst)
		   (t (let* ((k (random n)) (temp (nth n lst)))
			(setf (nth n lst) (nth k lst))
			(setf (nth k lst) temp)		    
			(shuffle-helper lst (1- n)))))))
  (shuffle-helper lst (1- (length lst)))))


(defun mount-board (lst)
  "Mounts the board froma list."
  (labels ((split-into-rows (lst row-size)
             (cond ((null lst) nil)
		   (t (cons (subseq lst 0 row-size)
			    (split-into-rows (nthcdr row-size lst) row-size))))))
    (cond ((and lst (= (length lst) 100)) (split-into-rows lst 10))
	  (t (error "Input list must contain 100 elements.")))))


(defun populate-positions-map (table)
  "Populate the map with all all the values and its positions."
  (dotimes (i 10)
    (dotimes (j 10)
      (let* ((value (nth j (nth i table)))
	     (position (list j i)))
	(if (not (null value))
	    (setf (gethash value positions-map) position))))))

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
	   (reverse moves)))))


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
	    (if parent-node parent-node nil))))


(defun create-next-node (current-state parent-node current-player)
  "Creates a new node."
  (cond ((and current-state parent-node
	      (or (equal current-player "player1") (equal current-player "player2")))
	 (let* ((player-coordinates (gethash current-player current-state))
	       (score (parse-integer
		       (nth (first player-coordinates)
			    (nth (second player-coordinates) board)))))
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


(defun get-node-state (node)
  (first node))


(defun get-node-score-player-one (node)
  (second node))


(defun get-node-score-player-two (node)
  (third node))


(defun get-node-parent (node)
  (fourth node))


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
    (remove-nil (knight-can-move-to (get-node-state current-node) current-player))))


;; ### Utils ###############################################################################

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


(defun get-doubles ()
  "Return a list with all doubles possible."
  '("99" "88" "77" "66" "55" "44" "33" "22" "11" "00"))


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

(defun max-line (line)
  "Returns the maximum value in the line"
  (let* ((numeric-values (mapcar #'(lambda(value)
                                     "Parses the values to int"
                                     (if (null value) 0 (parse-integer value))) 
                                 line))
         (maximum (apply #'max numeric-values)))
    (position maximum numeric-values)))


(defun initialize-game ()
  "Initializes both players position."
  (let ((player-one "player1")
	(player-two "player2")
	(player-one-position (list (max-line (first board)) 0))
	(player-two-position (list (max-line (tenth board)) 9))
	(root-state (make-hash-table :test 'equal)))
    (setf (gethash player-one root-state) nil)
    (setf (gethash player-two root-state) nil)
    (let* ((root (create-node root-state 0 0))
	   (first-node (create-next-node (knight-move-to (get-node-state root)
							 player-one-position
							 player-one)
					 root
					 player-one))
	   (second-node (create-next-node (knight-move-to (get-node-state first-node)
							  player-two-position
							  player-two)
					  first-node
					  player-two)))
      second-node)))

(defun evaluate (node player)
  "Returns the difference of the scores between the players."
  (let ((score1 (get-node-score-player1 node))
        (score2 (get-node-score-player2 node)))
  (cond ((equal player "player1") (- score1 score2))
        (t (- score2 score1)))))

(defun time-available(time-limit start)
  "Returns the percentage of time passed."
  (* (/ (- (get-internal-real-time) start) 1000.0 (/ time-limit 1000)) 100))
  
