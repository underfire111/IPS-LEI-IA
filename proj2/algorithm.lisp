;; ### Algorithm ############################################################################

(defparameter time-cap-percentage 90)

(defun alpha-beta (root player1 player2 depth)
  "Alpha Beta"
  (if (not (knight-can-move (get-node-state root) player1)) (return-from alpha-beta root))
  (let ((start-time (get-internal-real-time))
	(counter-nodes 0)
	(counter-min-cuts 0)
        (counter-max-cuts 0))
    (labels
	((max-layer (parent current-player depth alpha beta)
	   "Max layer in the algorithm."
	   (cond
	     ((or (zerop depth)
		  (>= (time-available max-time start-time) time-cap-percentage)
		  (not (knight-can-move (get-node-state parent) current-player)))
	      (incf counter-nodes)
	      (list parent (evaluate parent player1)))
	     (t (dolist (child (sort (partenogenese parent current-player)
				     #'(lambda (v1 v2) (> (evaluate v1 current-player)
							  (evaluate v2 current-player)))))
		  (let ((tmp (min-layer child player2 (1- depth) alpha beta)))
		    (setf alpha (if (>= (second alpha) (second tmp)) alpha tmp)))
		  (cond ((>= (second alpha) (second beta))
			 (incf counter-max-cuts)
			 (return-from max-layer beta))))
		alpha)))	 
	 (min-layer (parent current-player depth alpha beta)
	   "Max layer in the algorithm."
	   (cond
	     ((or (zerop depth)
		  (>= (time-available max-time start-time) time-cap-percentage)
		  (not (knight-can-move (get-node-state parent) current-player)))
              (incf counter-nodes)
	      (list parent (evaluate parent player1)))
	     (t (dolist (child (sort (partenogenese parent current-player)
				     #'(lambda (v1 v2) (> (evaluate v1 current-player)
							  (evaluate v2 current-player)))))
		  (let ((tmp (max-layer child player1 (1- depth) alpha beta)))
		    (setf beta (if (<= (second beta) (second tmp)) beta tmp)))
		  (cond ((>= (second alpha) (second beta))
		       (incf counter-min-cuts)
		       (return-from min-layer alpha))))
		beta))))
      (let ((result (max-layer root player1 depth (list 0 most-negative-fixnum)
			       (list 0 most-positive-fixnum))))
        (write-solution-to-file
	 (append result (list counter-nodes) (list counter-min-cuts) (list counter-max-cuts)
		 (list (* 1.0 (/ (- (get-internal-real-time) start-time)
			  internal-time-units-per-second)))))
        (get-move (first result) (get-node-depth (first result))
		  (1+ (get-node-depth root)))))))

