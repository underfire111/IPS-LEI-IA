;; positions-map	| key: "value"  value: (z y)
;; state-map		| key: (x y)    value: "#CODE" 

;; ### Board ##################################################################################

(defun list-positions ()
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


(defun shuffle-positions (lst)
  (labels ((shuffle-helper (lst n)
             (cond ((<= n 1) lst)
		   (t (let* ((k (random n)) (temp (nth n lst)))
			(setf (nth n lst) (nth k lst))
			(setf (nth k lst) temp)
			(shuffle-helper lst (1- n)))))))
    (shuffle-helper lst (1- (length lst)))))


(defun mount-board (lst)
  (labels ((split-into-rows (lst row-size)
             (cond ((null lst) nil)
                 (t (cons (subseq lst 0 row-size)
                       (split-into-rows (nthcdr row-size lst) row-size))))))
    (cond ((and lst (= (length lst) 100)) (split-into-rows lst 10))
	  (t (error "Input list must contain 100 elements.")))))


(defun populate-positions-map (table)
  (dotimes (i 10)
    (dotimes (j 10)
      (let* ((value (nth j (nth i table)))
	     (position (list j i)))
	(if (not (null value))
	    (setf (gethash value positions-map) position))))))

(defun get-problem ()
  "Returns the goal to the selected problem and initializes the board."
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


;; ### Utils ##################################################################################

(defun init-open-list(&optional calculate_fgh)
  "Initializes the open list of nodes"
  (let ((nodes '()))
    (mapcar #'(lambda(value)
		"Creates the nodes with the values"
	       (let* ((coordinates (gethash value positions-map))
		      (init-state (knight-start-position (first coordinates) (second coordinates)))
		      (child (create-node init-state)))
		 (cond ((null calculate_fgh) (push child nodes))
		       (t (let* ((h (funcall calculate_fgh child))
				 (f (list h 0 h)))
			(push (append child f) nodes))))))
	    (remove-nil (first board)))
    (reverse nodes)))

(defun clone-hash-table (table)
  "Creates a shallow copy of a hash table."
  (let ((new-table (make-hash-table
                    :test (hash-table-test table)
                    :size (hash-table-size table))))
    (maphash #'(lambda(key value)
                 (setf (gethash key new-table) value))
             table)
    new-table))

(defun reverse-string (str)
  "Reverse the characters in a string."
  (coerce (reverse (coerce str 'list)) 'string))

(defun remove-nil(lst)
  "Removes all the nil values of the list"
  (let ((values '()))
    (mapcar #'(lambda(value)
		(if (not (null value)) (push value values)))
	    lst)
    (reverse values)))

(defun join-numbers-as-string (num1 num2)
  "Join two numbers together as a string."
  (concatenate 'string (write-to-string num1) (write-to-string num2)))

(defun depth(node)
  "Calculates the depth of the node"
  (cond ((null node) 0)
	(t (1+ (depth (third node))))))

;;; (defun get-doubles-available)


;; ### Knight #################################################################################

(defun knight-start-position (x y)
  "Places the knight at the board"
  (let ((new-state (make-hash-table :test 'equal))
	 (value (nth x (nth y board))))
    (setf (gethash "kn" new-state) (list x y)) ;; set knight on board
    (setf (gethash (list x y) new-state) "#0") ;; add visited position
    (let ((temp (gethash (reverse-string value) positions-map)))
      (cond ((not (equal temp value))
	     (setf (gethash temp new-state) "#SYM"))
	    (t))) ;; else #DBL
    new-state))


(defun knight-can-move-to (current-state)
  "Check for all possible moves."
  (cond ((not (null (gethash "kn" current-state)))
	 (let ((x (first (gethash "kn" current-state)))
	       (y (second (gethash "kn" current-state)))
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


(defun knight-can-move (current-state)
  "Check if the knight still have any move available."
  (let ((next (knight-can-move-to current-state)))
    (not (notany #'identity next))))


(defun knight-move-to (current-state coordinates)
  (cond ((not (null current-state))
	 (cond (coordinates
		(let ((value (nth (first coordinates) (nth (second coordinates) board))))
		  (setf (gethash "kn" current-state) coordinates)
		  (setf (gethash coordinates current-state) "#MOV")
		  (let ((temp (gethash (reverse-string value) positions-map)))
		    (cond ((not (equal temp value))
			   (setf (gethash temp current-state) "#SYM"))
			  (t))))) ;; DBL
	       (t (error "Invalid move!")))
	 current-state)
	(t (error "Invalid state!"))))


(defun knight-do-move (current-state next-generation move)
  "Execute a valid knight move"
  (cond ((not (null current-state))
	 (let ((next (nth move next-generation)))
	   (cond (next
		  (let ((value (nth (first next) (nth (second next) board))))
		    (setf (gethash "kn" current-state) next)
		    (setf (gethash next current-state) (format nil "#~a" move))
		    (let ((temp (gethash (reverse-string value) positions-map)))
		      (format t "~a ~a~%" value (reverse-string value))
		      (cond ((not (equal temp value))
			     (setf (gethash temp current-state) "#SYM"))
			    (t))))) ;; DBL
		 (t (error "Invalid move!")))
	   current-state))
	(t (error "Invalid state!"))))



;; ### Node #################################################################################

;; node e uma lista com:
;; curr-state cur-score parent childs fgh
;;     1          2       3      4     5


(defun create-node (current-state &optional parent)
  (let* ((coordinates (gethash "kn" current-state))
	 (curr-score (parse-integer (nth (first coordinates) (nth (second coordinates) board)))))
    (list current-state
	  (if (not (null parent)) (+ curr-score (second parent)) curr-score)
	  (if (not (null parent)) parent nil)
	  '())))

(defun partenogenese(node &optional calculate_fgh)
  (mapcar #'(lambda(coordinates)
	      "Creates a node moving the knight to the coordinates"
	      (let ((child (create-node (knight-move-to (clone-hash-table (first node)) coordinates) node)))
		(cond ((not (null calculate_fgh))
		       (let* ((h (funcall calculate_fgh child))
			      (g (1+ (fifth node)))
			      (f (list (+ g h) g h)))
			 (append child f)))
		      (t child))))
	  (remove-nil (knight-can-move-to (first node)))))

;; ### Heuristic #################################################################################

(defun percentual-distance(node)
  "Calculates the percentage that the node has to get to be equal to score"
  (* 100 (/ (second node) score)))

(defun enunciation-heuristic(node)
  "Calculates the average points of the board and divides it by the number of points to be at the objective"
  (labels ((get-keys(map)
	     "Returns a list with only the values in the board as strings"
	     (let ((keys '()))
	       (maphash #'(lambda(k v) (push k keys)) map)
	       keys))
	   (get-sum-values(list-keys)
	     "Returns the sum of the values in the list"
	     (reduce #'(lambda(v1 v2)
			 (cond ((or (equal "kn" v1) (equal "kn" v2) (null v1) (null v2)) 0)
			       ((listp v1) (+ (parse-integer (nth (first v1) (nth (second v1) board))) (parse-integer (nth (first v2) (nth (second v2) board)))))
			       ((listp v2) (+ v1 (parse-integer (nth (first v2) (nth (second v2) board)))))
			       ((stringp v1) (+ (parse-integer v1) (parse-integer v2)))
			       (t (+ v1 (parse-integer v2)))))
		     list-keys)))
    (let* ((all-values (get-keys positions-map))
	   (removed-values (get-keys (first node)))
	   (length-list (- (length all-values) (length removed-values)))
	   (aux-score (- score (second node))))
      (if (= 0 aux-score) (setf aux-score 1))
      (if (= 0 length-list) (setf length-list 1))
      (/ (/ (- (get-sum-values all-values) (get-sum-values removed-values)) length-list) aux-score))))

;; ### Metrics #################################################################################

(defun penetrance(len-path total-number-nodes)
  "Calculates the penetrance"
  (format t "Penetrance: ~a~%" (/ len-path total-number-nodes)))

;; Branching factor


