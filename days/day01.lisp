;; Put the given solutions for the examples here
(setf test-sol-a 24000)
(setf test-sol-b 45000)

(defun parse-input (input-file)
  ;;; Splits on double newlines, then splits on single newlines
  (mapcar (lambda (s) (mapcar 'parse-integer (split #\Newline s)))
	  (split (format nil "~%~%") (get-file-string input-file))))

(defmacro solve (input-list length)
  ;;; Creates a function that sums the lists, sorts them and return the first n elements
  `(apply '+ (subseq (sort (mapcar (lambda (l) (apply '+ l)) ,input-list) '>) 0 ,length)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (solve parsed-input 1))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (solve parsed-input 3))
