;; ### Algorithm ############################################################################

(defun alpha-beta-a (root player1 player2 &key (depth 5))
  "Alpha Beta"
  (if (not (knight-can-move (get-node-state root) player1)) (return-from alpha-beta-a root))
  (let ((start-time (get-internal-real-time)))
    (labels
	((max-layer (parent current-player depth alpha beta)
	   "Max layer in the algorithm."
	   (cond
	     ((or (zerop depth) (>= (time-available max-time start-time) 90)
		  (not (knight-can-move (get-node-state parent) current-player)))
	      (list parent (evaluate parent player1)))
	     (t (dolist (child (sort (partenogenese parent current-player)
				     #'(lambda (v1 v2) (> (evaluate v1 current-player)
							   (evaluate v2 current-player)))))
		  (let ((tmp (min-layer child player2 (1- depth) alpha beta)))
		    (setf alpha (if (>= (second alpha) (second tmp)) alpha tmp)))
		  (if (>= (second alpha) (second beta)) (return-from max-layer beta)))
		alpha)))
	 (min-layer (parent current-player depth alpha beta)
	   "Max layer in the algorithm."
	   (cond
	     ((or (zerop depth) 
		  (>= (time-available max-time start-time) 90)
		  (not (knight-can-move (get-node-state parent) current-player)))
	      (list parent (evaluate parent player1)))
	     (t (dolist (child (sort (partenogenese parent current-player)
				     #'(lambda (v1 v2) (> (evaluate v1 current-player)
							  (evaluate v2 current-player)))))
		  (let ((tmp (max-layer child player1 (1- depth) alpha beta)))
		    (setf beta (if (<= (second beta) (second tmp)) beta tmp)))
		  (if (>= (second alpha) (second beta)) (return-from min-layer alpha)))
		beta))))
      (let ((result (max-layer root player1 depth (list 0 most-negative-fixnum)
			       (list 0 most-positive-fixnum))))
        (get-move (first result) (get-node-depth (first result))
		  (1+ (get-node-depth root)))))))


(defun alpha-beta-b (root player1 player2 &key (depth 5))
  "Alpha Beta"
  (if (not (knight-can-move (get-node-state root) player1)) (return-from alpha-beta-b root))
  (let ((start-time (get-internal-real-time)))
    (labels
	((max-layer (parent current-player depth alpha beta)
	   "Max layer in the algorithm."
	   (cond
	     ((or (zerop depth) (>= (time-available max-time start-time) 90)
		  (not (knight-can-move (get-node-state parent) current-player)))
	      (list parent (evaluate parent player1)))
	     (t (dolist (child (sort (partenogenese parent current-player)
				     #'(lambda (v1 v2)
					 (> (potato-logic v1 player1 player2)
					    (potato-logic v2 player1 player2)))))
		  (let ((tmp (min-layer child player2 (1- depth) alpha beta)))
		    (setf alpha (if (>= (second alpha) (second tmp)) alpha tmp)))
		  (if (>= (second alpha) (second beta)) (return-from max-layer beta)))	        
		alpha)))
	 (min-layer (parent current-player depth alpha beta)
	   "Max layer in the algorithm."
	   (cond
	     ((or (zerop depth) 
		  (>= (time-available max-time start-time) 90)
		  (not (knight-can-move (get-node-state parent) current-player)))
	      (list parent (evaluate parent player1)))
	     (t (dolist (child (sort (partenogenese parent current-player)
				     #'(lambda (v1 v2)
					 (> (potato-logic v1 player2 player1)
					    (potato-logic v2 player2 player1)))))
		  (let ((tmp (max-layer child player1 (1- depth) alpha beta)))
		    (setf beta (if (<= (second beta) (second tmp)) beta tmp)))
		  (if (>= (second alpha) (second beta)) (return-from min-layer alpha)))
		beta))))
      (let ((result (max-layer root player1 depth (list 0 most-negative-fixnum)
			       (list 0 most-positive-fixnum))))
        (get-move (first result) (get-node-depth (first result))
		  (1+ (get-node-depth root)))))))

(defun get-move (current-node current-depth root-depth)
  "Gets the node above height layers of node" 
  (cond ((equal root-depth current-depth) current-node)
        (t (get-move (get-node-parent current-node) (1- current-depth) root-depth))))

