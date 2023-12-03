#####################################################################################

;; BFS


(defun breadth-first-search (start-node)
  "Performs Breadth-First Search on a graph starting from the given node."
  (let ((visited (make-hash-table)))
    (labels ((enqueue (queue node)
               (push node queue))
             (dequeue (queue)
               (pop queue))
             (visit-node (node)
	       (setf (gethash node visited) t))
             (enqueue-childs (node queue) ;; that function shall create the child nodes
	       
	       )
             (bfs-traversal (queue)
               (when queue
                 (let ((current-node (dequeue queue)))
                   (visit-node current-node)
                   (enqueue-childs current-node queue)
                   (bfs-traversal queue)))))
      (bfs-traversal (list start-nod1e)))))


#####################################################################################

;; DFS
(defun breadth-first-search (node)
  
  )


#####################################################################################

;; A*
(defun breadth-first-search (node)
  
  )

