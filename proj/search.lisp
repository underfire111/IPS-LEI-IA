;; Generic Search Algorithm

(defun commun-algorithm (ft-fix-open-list ft-init-list-opened-nodes &optional heuristic)
  "Common algorithm between BFS, DFS and A*."
  (let ((counter-closed-nodes 0)
	(list-opened-nodes (funcall ft-init-list-opened-nodes)))
    (let ((is-solution (validate-childs list-opened-nodes)))
      (if (not (null is-solution))
	  (return-from commun-algorithm (list is-solution (1+ (length list-opened-nodes))))))
    (labels
	((recursive-algorithm ()
	   (incf counter-closed-nodes)
	   (let* ((node (pop list-opened-nodes))
		  (childs (partenogenese node heuristic)))
	     (let ((is-solution (validate-childs childs)))
	       (if is-solution (return-from recursive-algorithm is-solution)))
	     (setf list-opened-nodes (funcall ft-fix-open-list list-opened-nodes childs)))
	   (if list-opened-nodes (recursive-algorithm))))
      (if list-opened-nodes
	  (list (recursive-algorithm)
		(+ 1 counter-closed-nodes (length list-opened-nodes)))))))

;; ### BFS #################################################################################

(defun breadth-first-search ()
  "Performs Breadth-First Search on a graph starting from the given node."
  (commun-algorithm #'(lambda (list-opened-nodes childs)
			(append list-opened-nodes childs))
		    #'init-list-opened-nodes))


;; ### DFS #################################################################################

(defun depth-first-search ()
  "Performs Depth-First Search on a graph."
  (commun-algorithm #'
   (lambda (list-opened-nodes childs)
     (append childs list-opened-nodes))
   #'init-list-opened-nodes))


;; ### A* ##################################################################################

(defun a-star (heuristic)
  "Performs A* on a graph."
  (commun-algorithm
   #'(lambda (list-opened-nodes childs)
       (sort-list-opened-nodes-ascending (append list-opened-nodes childs)))
   #'(lambda ()
       (sort-list-opened-nodes-ascending (init-list-opened-nodes heuristic)))
   heuristic))

;; ### IDA* ################################################################################


#|

(defun ida* (heuristic)
  "Performs IDA on a graph."
  (let*
      ((counter-closed-nodes 0)
       (list-opened-nodes
	 (sort-list-opened-nodes-ascending (init-list-opened-nodes heuristic)))
       (max-cost
         (apply #'min (mapcar #'first (mapcar #'get-node-fgh list-opened-nodes)))))
    (let ((is-solution (validate-childs list-opened-nodes)))
      (if is-solution
	  (return-from ida* (list is-solution (1+ (length list-opened-nodes))))))
    (labels
	((recursive-algorithm ()
	   (incf counter-closed-nodes)
	   (cond
	     ((null list-opened-nodes) nil)
	     ((<= (first (get-node-fgh (first list-opened-nodes))) max-cost)
	      (let* ((node (pop list-opened-nodes))
		     (childs (partenogenese node heuristic)))
		(let ((is-solution (validate-childs childs)))
		  (if is-solution (return-from recursive-algorithm is-solution)))
		(setf list-opened-nodes
		      (sort-list-opened-nodes-ascending (append list-opened-nodes childs))))
	      (if list-opened-nodes (recursive-algorithm)))
	     (t (setf max-cost (first (get-node-fgh (first list-opened-nodes))))
		(setf counter-closed-nodes 0)
		(setf list-opened-nodes
		      (sort-list-opened-nodes-ascending (init-list-opened-nodes heuristic)))
		(recursive-algorithm)))))
      (if list-opened-nodes
	  (list (recursive-algorithm)
		(+ 1 counter-closed-nodes (length list-opened-nodes)))))))
|#

(defun iterative-depending-a-star (heuristic)
  "Performs IDA on a graph."
  (let ((bound most-positive-fixnum)
	(list-opened-nodes '(root))
	(counter-closed-nodes 0))
    (labels
	((recursive-algorithm ()
	   (let ((node (first list-opened-nodes))
		 (min most-negative-fixnum))
	     (if (not (equal node 'root))
		 (cond ((> (first (get-node-fgh node)) bound)
			(return-from recursive-algorithm (first (get-node-fgh node))))
		       ((>= (get-node-score node) score)
			(return-from recursive-algorithm 'found)))
		 (pop list-opened-nodes))
	     (let* ((successors
		      (sort-list-opened-nodes-ascending
		       (partenogenese node heuristic))))
	       (setf counter-closed-nodes (+ counter-closed-nodes (length successors)))
	       (loop for node in successors
		     do (cond
			  ((not (member node list-opened-nodes))
			   (push node list-opened-nodes)
			   (let ((temp (recursive-algorithm)))
			     (cond ((equal temp 'found)
				    (return-from recursive-algorithm 'found))
				   ((< temp min) (setf min temp)))
			     (pop list-opened-nodes)))))
	       (return-from recursive-algorithm min)))))
      (loop
	(let ((temp (recursive-algorithm)))
	  (unless (not (equal temp 'found))
	    (return (list (first list-opened-nodes) (1+ counter-closed-nodes))))
	  (unless (not (= temp most-negative-fixnum))
	    (return (list nil (1+ counter-closed-nodes))))
	  (setf bound temp))))))
  
;; ### SMA* ################################################################################


(defun simplified-memory-bounded-a-star (heuristic &optional (limit 10))
  "Performs SMA* on a graph."
  (commun-algorithm
   #'(lambda (list-opened-nodes childs)
       (let ((temp (append list-opened-nodes childs)))
	 (subseq (sort-list-opened-nodes-ascending temp) 0
		 (min (length temp) limit))))
   #'(lambda ()
       (let ((temp (init-list-opened-nodes heuristic)))
	 (subseq (sort-list-opened-nodes-ascending temp) 0
		 (min (length temp) limit))))
   heuristic))


;; ### RBFS ################################################################################

(defun element-in-list-p (element list)
  "Check if the list contains the given element using member."
  (member element list :test #'equal))

(defun update-parent (node children)
  "Update the parent node scores based on the scores of the children nodes."
  (cond ((null (get-node-parent node)) nil)
        (t 
         (setf (first (get-node-fgh (get-node-parent node)))
               (apply #'min (mapcar #'(lambda (value) (first (get-node-fgh value))) children))))))


(defun rbfs(list-open-nodes node heuristic)
  (print-current-state node)
  "Recursive Best-First Search algorithm."
  (cond ((>= (get-node-score node) score) node)
        ((and (null list-open-nodes) (null node)) nil)
        (t (let* ((children (partenogenese node heuristic))
                  (new-lst-open (sort-open-list-ascending (append (rest list-open-nodes) children)))
                  (next-node (first new-lst-open)))
             (print list-open-nodes)
             (print children)
           (cond ((and next-node (element-in-list-p next-node children))
                  (let ((solution (rbfs new-lst-open next-node heuristic)))
                    (cond ((>= (get-node-score solution) score) solution)
                          (t (update-parent node children)))))
                 (t (update-parent node children)))))))
                  
