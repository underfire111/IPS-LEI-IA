

;;; Input Functions

(defun getNumber()
  "Receives an input from the user and guarantees that it is a number"
    (let ((value (read)))
      (if (numberp value) (return-from getNumber value)))
  (print "The answer must be a number!
Please choose a number as an option:") 
  (getNumber))


;;; Load Functions

(defun loadBoards(filename)
  "Reads the filename and loads every board in it"
  (let ((result '())
        (current_problem '())
        (current_board '()))
    (with-open-file (file filename :direction :input :if-does-not-exist :error)
      (loop for line = (read-line file nil)
            while line
            do
              (cond ((char= #\P (char line 0))
                     (setf current_problem (list (subseq line (1+ (position #\Space line)) (1- (length line)))))) ;; Gets the problem's name
                    ((char= #\O (char line 0)) ;; Gets its objective
                     (push (getObjective line) current_problem))
                    ((char= #\- (char line 0)) ;; Separator between problems
                     (push (reverse (copy-list current_board)) current_problem) ;; stores current problem
                     (push (reverse (copy-list current_problem)) result)
                     (setf current_board '()) ;; Resets variables
                     (setf current_problem '()))
                    (t 
                     (push (getBoard line) current_board))))) ;; Gets the current board
    (reverse result)))

(defun getObjective(line)
  "Parses the objetive to integer if it is a number, else returns random"
  (let ((objective (subseq line (position #\Space line) (length line))))
    (cond ((char= #\? (char objective 1)) 'random)
          (t (parse-integer objective)))))


(defun getBoard(line)
  "Transforms the current line of the board into a list. If the line says random, it will return the string random"
  (cond ((char= #\r (char line 0)) 'random)
        (t 
         (loop for start = 0 then (1+ end)
               for end = (or (position #\Space line :start start) (1- (length line)))
               while (and end (< start (length line)))
               collect (let ((c (subseq line start (1+ end))))
                         (cond ((or (equal c "NIL") (equal c "NIL ")) nil)
                               (t (parse-integer c))))))))


;;; Output Functions

(defun printRow(line)
  "Prints a string"
  (cond ((null line) nil)
        (t (format t "~a " (first line))
           (printRow (rest line)))))

(defun printBoard(board)
  "Prints the board"
  (cond ((null board) nil)
        ((equal 'random (first board)) (format t "Random~%"))
        (t
         (printRow (first board))
         (format t "~%")
         (printBoard (rest board)))))

(defun printInformationOfBoards(boards)
  "Prints the informations about every board"
  (cond ((null boards) nil)
        ((stringp (first boards)) 
         (format t "Problem ~a:~%" (first boards))
         (printInformationOfBoards (rest boards)))
        ((or (numberp (first boards)) (equal 'random (first boards))) 
         (format t "Objective: ~a~%" (first boards))
         (printInformationOfBoards (rest boards)))
        (t
         (format t "Board:~%")
         (printBoard (first boards))
         (format t  "-----------------------------~%")
         (printInformationOfBoards (rest boards)))))

(defun printListOfBoards(boards)
  "Print the list of all the boards available"
  (cond ((null boards) nil)
        (t (printInformationOfBoards (first boards))
           (printListOfBoards (rest boards)))))


;;; Main

(defun test(path)
    (let* ((file (concatenate 'string path "/problemas.dat"))
           (boards (loadBoards file)))
      (printListOfBoards boards)
      (format t "Choose the problem(the choice must be a number): ")
      (let ((option (getNumber)))
        (cond ((or (< option 1) (>= option (length boards))) 'random)
              (t 
               (printInformationOfBoards (nth (1- option) boards)))))))

