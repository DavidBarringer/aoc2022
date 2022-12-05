;; Put the given solutions for the examples here
(setf test-sol-a "CMZ")
(setf test-sol-b "MCD")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let* ((sep (split (format nil "~%~%") (get-file-string input-file)))
	 (cranes (split #\newline (CAR sep)))
	 (instructions (split #\newline (CADR sep))))
    (list
     (mapcar (lambda (l) (remove (CAR (last l)) (remove " "  l :test 'string=))) (apply 'mapcar 'list (mapcar (lambda (s) (separate 1 2 s)) cranes)))
     (mapcar (lambda (s1) (remove "" (split "\\D+" s1) :test 'string=)) instructions))))

(defun separate (pos trim string)
  (if (>= (+ pos trim) (length string)) (list (format nil "~C" (char string pos)))
      (cons (format nil "~C" (char string pos)) (separate pos trim (subseq string (+ pos 1 trim))))))

(defstruct crane crates)

(defun build-cranes (crane-list)
  (loop for crane in crane-list
	for i from 1
	for c = (intern (format nil "~D" i))
	do (setf (symbol-value c) (make-crane :crates crane))
	collect c into res
	finally (return res)))


(defun move (num crane-1 crane-2)
  (setf (crane-crates crane-2) (append (subseq (crane-crates crane-1) 0 num) (crane-crates crane-2)))
  (setf (crane-crates crane-1) (nthcdr num (crane-crates crane-1))))


;; Returns the solution for part a
(defun part-a (parsed-input)
  (let ((crane-list (build-cranes (CAR parsed-input))))
    (loop for instruction in (CADR parsed-input)
	  do (loop for i from 1 to (parse-integer (CAR instruction))
		   do (move 1
			    (symbol-value (intern (CADR instruction)))
			    (symbol-value (intern (CADDR instruction))))))
    (format nil "~{~A~}" (mapcar 'car (loop for crane in crane-list collect (crane-crates (symbol-value crane)))))))
    


;; Returns the solution for part b
(defun part-b (parsed-input)
  (let ((crane-list (build-cranes (CAR parsed-input))))
    (loop for instruction in (CADR parsed-input)
	  do (move (parse-integer (CAR instruction))
		   (symbol-value (intern (CADR instruction)))
		   (symbol-value (intern (CADDR instruction)))))
    (format nil "~{~A~}" (mapcar 'car (loop for crane in crane-list collect (crane-crates (symbol-value crane)))))))
