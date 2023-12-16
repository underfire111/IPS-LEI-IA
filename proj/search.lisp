;; Generic Search Algorithm

(defun commun-algorithm (ft-fix-open-list ft-init-open-list &optional heuristic)
  "Common algorithm between BFS, DFS and A*"
  (let ((counter-nodes-closed 0)
	(list-open-nodes (funcall ft-init-open-list)))
    (let ((is-solution (validate-childs list-open-nodes)))
      (if (not (null is-solution)) (return-from commun-algorithm is-solution)))
    (labels
	((recursive-algorithm ()
	   (incf counter-nodes-closed)
	   (let* ((node (pop list-open-nodes))
		  (childs (partenogenese node heuristic)))
	     (let ((is-solution (validate-childs childs)))
	       (if (not (null is-solution)) (return-from commun-algorithm is-solution)))
	     (setf list-open-nodes (funcall ft-fix-open-list list-open-nodes childs)))
	     (if (not (null list-open-nodes)) (recursive-algorithm))))
      (if (not (null list-open-nodes)) 
	  (let ((result (recursive-algorithm)))
	    (penetrance
	     (get-node-depth result)
	     (+ counter-nodes-closed (length list-open-nodes)))
	    result)))))


;; ### BFS #################################################################################

(defun breadth-first-search ()
  "Performs Breadth-First Search on a graph starting from the given node."
  (commun-algorithm #'(lambda (list-open-nodes childs)
	    (append list-open-nodes childs))
	#'init-open-list))


;; ### DFS #################################################################################

(defun depth-first-search ()
   "Performs Depth-First Search on a graph."
  (commun-algorithm #'
   (lambda (list-open-nodes childs)
     (append childs list-open-nodes))
   #'init-open-list))


;; ### A* ##################################################################################

(defun a* (heuristic)
  "Performs A* on a graph."
  (commun-algorithm
   #'(lambda (list-open-nodes childs)
       (sort-open-list-ascending (append list-open-nodes childs)))
   #'(lambda ()
       (sort-open-list-ascending (init-open-list heuristic)))
   heuristic))

;; ### IDA* ################################################################################

(defun ida* (heuristic)
  "Performs IDA on a graph"
  (let*
      ((counter-nodes-closed 0)
       (list-open-nodes (sort-open-list-ascending (init-open-list heuristic)))
       (max-cost (apply #'min (mapcar #'first (mapcar #'get-node-fgh list-open-nodes)))))
    (let ((is-solution (validate-childs list-open-nodes)))
      (if is-solution (return-from ida* is-solution)))
    (labels
	((algorithm ()
	   (incf counter-nodes-closed)
	   (cond
	     ((null list-open-nodes) nil)
	     ((<= (first (get-node-fgh (first list-open-nodes))) max-cost)
	      (let* ((node (pop list-open-nodes))
		     (childs (partenogenese node heuristic)))
		(let ((is-solution (validate-childs childs)))
		  (if is-solution (return-from algorithm is-solution)))
		(setf list-open-nodes
		      (sort-open-list-ascending (append list-open-nodes childs))))
		    (if (not (null list-open-nodes)) (algorithm)))
	     (t
	      (setf max-cost (first (get-node-fgh (first list-open-nodes))))
	      (setf counter-nodes-closed 0)
	      (setf list-open-nodes
		    (sort-open-list-ascending (init-open-list heuristic)))
	      (algorithm)))))
      (if (not (null list-open-nodes)) 
	  (let ((result (algorithm)))
	    (penetrance
	     (get-node-depth result)
	     (+ counter-nodes-closed (length list-open-nodes)))
	    result)))))
	     
;; ### SMA* ################################################################################


(defun sma* (heuristic &optional (limit 10))
  "Performs A* on a graph."
  (commun-algorithm
   #'(lambda (list-open-nodes childs)
       (let ((temp (append list-open-nodes childs)))
	 (subseq (sort-open-list-ascending temp) 0
		 (min (length temp) limit))))
   #'(lambda ()
       (let ((temp (init-open-list heuristic)))
	 (subseq (sort-open-list-ascending temp) 0
		 (min (length temp) limit))))
   heuristic))
