;; Generic Search Algorithm
(defun algo(fun init-open-list-func &optional heuristic)
  "Algorithm to the BFS and DFS"
  (let ((counter-nodes-closed 0)
	(list-open-nodes (funcall init-open-list-func)))
    (labels ((recursive-algorithm(root)
	       ;; (print-final-result root)
	       (incf counter-nodes-closed)
	       (cond ((<= score (second root)) root)
		     (t (setf list-open-nodes (funcall fun list-open-nodes (partenogenese root heuristic)))
			(if (not (null list-open-nodes)) (recursive-algorithm (pop list-open-nodes)))))))
      (let ((result (recursive-algorithm (pop list-open-nodes))))
	(penetrance (depth result) (+ counter-nodes-closed (length list-open-nodes)))
	result))))

;; BFS
(defun bfs()
  "Performs Breadth-First Search on a graph starting from the given node."
  (algo #'(lambda(list-open-nodes childs)
	    (append list-open-nodes childs))
	#'init-open-list))



;; DFS
(defun dfs()
   "Performs Depth-First Search on a graph."
  (algo #'(lambda(list-open-nodes childs)
	    (append childs list-open-nodes))
	#'init-open-list))

;; A*
(defun a*(heuristic sort-func)
  "Performs A* on a graph. The heuristic of the algorithm is the one given by parameter and the sort function given will sort the open nodes's list, so that it's known which node to get first."
  (algo #'(lambda(list-open-nodes childs)
	    (sort (append list-open-nodes childs) sort-func))
	#'(lambda()
	    (sort (init-open-list heuristic) sort-func))
	heuristic))
