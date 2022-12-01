;; Put the given solutions for the examples here
(setf test-sol-a 24000)
(setf test-sol-b 45000)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (mapcar 'parse-integer (split #\Newline s)))
	  (split (format nil "~%~%") (get-file-string input-file))))

(defmacro make-func (input-list length)
  `(apply '+ (subseq (sort (mapcar 'eval (mapcar (lambda (l) (cons '+ l)) ,input-list)) '>) 0 ,length)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (make-func parsed-input 1))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (make-func parsed-input 3))
