;; Put the given solutions for the examples here
(setf test-sol-a 2)
(setf test-sol-b 4)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (mapcar (lambda (s2) (mapcar 'parse-integer (split "-" s2))) (split "," s))) (get-file-lines input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (loop for ((v w) (x y)) in parsed-input
	count (OR (AND (<= v x) (>= w y))
		  (AND (<= x v) (>= y w)))))


;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for ((v w) (x y)) in parsed-input
	count (OR (AND (<= v x) (>= w x))
		  (AND (<= x v) (>= y v)))))

