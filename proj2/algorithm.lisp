(defun alpha-beta (root player1 player2 &optional (time-limit 1000) (depth 5))
  "Alpha Beta"
  (let ((start (get-internal-real-time)))
    (labels ((max-layer(parent p1 depth alpha beta)
               "Max layer in the algorithm."
               (cond ((or (equal depth 0) (>= (time-available time-limit start) 90)) (evaluate node p1))
                     (t (dolist (child (sort (partenogenese parent player1) #'(lambda(v1 v2) (>= (evaluate v1 p1) (evaluate v2 p1)))))
                          (setf alpha (max alpha (min-layer child player2 (1- depth) alpha beta)))
                          (print "AEIOU")
                          (if (>= alpha beta) (return-from max-layer beta)))
                        alpha)))
             (min-layer(parent p1 depth alpha beta)
               "Max layer in the algorithm."
               (cond ((or (equal depth 0) (>= (time-available time-limit start) 90)) (evaluate node p1))
                     (t (dolist (child (sort (partenogenese parent player1) #'(lambda(v1 v2) (<= (evaluate v1 p1) (evaluate v2 p1)))))
                          (setf beta (min beta (max-layer child player1 (1- depth) alpha beta)))
                          (if (>= alpha beta) (return-from min-layer alpha)))
                        beta)))                                                
             (max-layer root player1 depth most-negative-fixnum most-positive-fixnum)))))


