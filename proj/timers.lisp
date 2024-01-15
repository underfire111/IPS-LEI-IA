
(let ((start-time (get-internal-real-time)))
  (test-simplified-memory-bounded-a-star #'calc-movements-left)
  (let ((end-time (get-internal-real-time)))
    (format t "(1) Execution time: ~,10F seconds ~%"
	    (/ (- end-time start-time) internal-time-units-per-second))))

(let ((start-time (get-internal-real-time)))
  (test-simplified-memory-bounded-a-star #'calc-percentual-distance)
  (let ((end-time (get-internal-real-time)))
    (format t "(2) Execution time: ~,10F seconds ~%"
	    (/ (- end-time start-time) internal-time-units-per-second))))

(let ((start-time (get-internal-real-time)))
  (test-simplified-memory-bounded-a-star #'calc-average-progression)
  (let ((end-time (get-internal-real-time)))
    (format t "(3) Execution time: ~,10F seconds ~%"
	    (/ (- end-time start-time) internal-time-units-per-second))))


(let ((start-time (get-internal-real-time)))
  (test-iterative-depending-a-star #'calc-movements-left)
  (let ((end-time (get-internal-real-time)))
    (format t "(1) Execution time: ~,10F seconds ~%"
	    (/ (- end-time start-time) internal-time-units-per-second))))

(let ((start-time (get-internal-real-time)))
  (test-iterative-depending-a-star #'calc-percentual-distance)
  (let ((end-time (get-internal-real-time)))
    (format t "(2) Execution time: ~,10F seconds ~%"
	    (/ (- end-time start-time) internal-time-units-per-second))))

(let ((start-time (get-internal-real-time)))
  (test-iterative-depending-a-star #'calc-average-progression)
  (let ((end-time (get-internal-real-time)))
    (format t "(3) Execution time: ~,10F seconds ~%"
	    (/ (- end-time start-time) internal-time-units-per-second))))


(let ((start-time (get-internal-real-time)))
  (test-a-star #'calc-movements-left)
  (let ((end-time (get-internal-real-time)))
    (format t "(1) Execution time: ~,10F seconds ~%"
	    (/ (- end-time start-time) internal-time-units-per-second))))

(let ((start-time (get-internal-real-time)))
  (test-a-star #'calc-percentual-distance)
  (let ((end-time (get-internal-real-time)))
    (format t "(2) Execution time: ~,10F seconds ~%"
	    (/ (- end-time start-time) internal-time-units-per-second))))

(let ((start-time (get-internal-real-time)))
  (test-a-star #'calc-average-progression)
  (let ((end-time (get-internal-real-time)))
    (format t "(3) Execution time: ~,10F seconds ~%"
	    (/ (- end-time start-time) internal-time-units-per-second))))
