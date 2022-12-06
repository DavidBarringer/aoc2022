;; Put the given solutions for the examples here
(setf test-sol-a 7)
(setf test-sol-b 19)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (concatenate 'list (get-file-string input-file)))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (CAR (loop for (v w x y) on parsed-input until (null y)
	     for i from 4
	     if (= (length (list v w x y))
		   (length (remove-duplicates (list v w x y))))
	       collect i)))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (CAR (loop for i from 0
	     until (= (+ i 14) (length parsed-input))
	     for l = (subseq parsed-input i (+ i 14))
	     if (= (length l)
		   (length (remove-duplicates l)))
	       collect (+ i 14))))
