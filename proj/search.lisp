;; Generic Search Algorithm

(defun commun-algorithm (algorithm-funtion init-open-list-func &optional heuristic)
  "Algorithm to the BFS and DFS"
  (let ((counter-nodes-closed 0)
	(list-open-nodes (funcall init-open-list-func)))
    (let ((is-solution (validate-childs list-open-nodes)))
      (if (not (null is-solution)) (return-from commun-algorithm is-solution)))
    (labels
	((recursive-algorithm ()
	   (incf counter-nodes-closed)
	   (let* ((node (pop list-open-nodes))
		  (childs (partenogenese node heuristic)))
	     (let ((is-solution (validate-childs childs)))
	       (if (not (null is-solution)) (return-from commun-algorithm is-solution)))
	     (setf list-open-nodes (funcall algorithm-funtion list-open-nodes childs))
	     (if (not (null list-open-nodes)) (recursive-algorithm)))))
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

(defun a* (heuristic sort-function)
  "Performs A* on a graph. The heuristic of the algorithm is the one given by parameter and the sort function given will sort the open nodes's list, so that it's known which node to get first."
  (commun-algorithm #'
   (lambda (list-open-nodes childs)
     (sort (append list-open-nodes childs) sort-function))
   #'(lambda ()
       (sort (init-open-list heuristic) sort-function))
   heuristic))
