;; ### Algorithm ##############################################################################

(defun alpha-beta (root player1 player2 &key (time-limit 1000) (depth 5))
  "Alpha Beta"
  (let ((start (get-internal-real-time)) (best-move nil))
    (labels
	((max-layer (parent p1 depth alpha beta)
	   "Max layer in the algorithm."
	   (cond ((or (zerop depth)
		      (>= (time-available time-limit start) 90)
		      kn)
		  (setf best-move parent)
		  (evaluate parent player1))
		 (t (dolist (child
			     (sort (partenogenese parent p1)
				   #'(lambda(v1 v2) (<= (evaluate v1 p1) (evaluate v2 p1)))))
		      (setf alpha (max alpha (min-layer child player2 (1- depth) alpha beta)))
		      (if (>= alpha beta)
			  (progn (setf best-move child) (return-from max-layer beta))))
		    alpha)))
	 (min-layer (parent p1 depth alpha beta)
	   "Max layer in the algorithm."
	   (cond ((or (zerop depth) (>= (time-available time-limit start) 90)) 
		  (setf best-move parent)
		  (evaluate parent player1))
		 (t (dolist (child
			     (sort (partenogenese parent p1)
				   #'(lambda(v1 v2) (<= (evaluate v1 p1) (evaluate v2 p1)))))
		      (setf beta (min beta (max-layer child player1 (1- depth) alpha beta)))
		      (if (>= alpha beta)
			  (progn (setf best-move child) (return-from min-layer alpha))))
		    beta))))                                                
      (max-layer root player1 depth most-negative-fixnum most-positive-fixnum))))

(defun get-move(current-node current-depth root-depth)
  "Gets the node above height layers of node" 
  (cond ((equal root-depth current-depth) current-node)
        (t (get-move (get-node-parent current-node) (1- current-depth) root-depth))))


