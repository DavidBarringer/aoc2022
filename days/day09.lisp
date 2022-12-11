;; Put the given solutions for the examples here
(setf test-sol-a 13)
(setf test-sol-b "No test solution")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (l) (cons (CAR l) (parse-integer (CADR l)))) (mapcar (lambda (s) (split #\  s)) (get-file-lines input-file))))

(defun move-head (knots times offset)
  (loop for i from 1 to times
	do (set (intern "knot0") (add-coords (symbol-value (intern "knot0")) offset))
	   (loop for i from 1 to knots
		 for knot = (intern (format nil "knot~D" i))
		 for knot-prev = (symbol-value (intern (format nil "knot~D" (- i 1))))
		 do (set knot (move-tail knot-prev (symbol-value knot))))
	collect (symbol-value (intern (format nil "knot~D" knots)))))

(defun R (knots times) (move-head knots times '(1 . 0)))
(defun L (knots times) (move-head knots times '(-1 . 0)))
(defun U (knots times) (move-head knots times '(0 . 1)))
(defun D (knots times) (move-head knots times '(0 . -1)))

(defun add-coords (c1 c2)
  (cons (+ (CAR c1) (CAR c2)) (+ (CDR c1) (CDR c2))))

(defun move-tail (head tail)
  (let ((x (- (CAR head) (CAR tail)))
	(y (- (CDR head) (CDR tail))))
    (if (AND (>= 1 (abs x)) (>= 1 (abs y))) tail
	(add-coords tail (cons (to-offset x) (to-offset y))))))

(defun to-offset (diff)
  (if (= 0 diff) 0 (/ diff (abs diff))))
	  
;; Returns the solution for part a
(defun part-a (parsed-input)
  (set (intern "knot0") '(0 . 0))
  (set (intern "knot1") '(0 . 0))
  (length (remove-duplicates (loop for (dir . times) in parsed-input
				   nconc (funcall (intern dir) 1 times)) :test 'equal)))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for i from 0 to 9
	do (set (intern (format nil "knot~D" i)) '(0 . 0))
	   (print (symbol-value (intern (format nil "knot~D" i)))))
  (length (remove-duplicates (loop for (dir . times) in parsed-input
				   nconc (funcall (intern dir) 9 times)) :test 'equal)))

