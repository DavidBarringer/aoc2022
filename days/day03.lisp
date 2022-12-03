;; Put the given solutions for the examples here
(setf test-sol-a 157)
(setf test-sol-b 70)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (concatenate 'list s)) (get-file-lines input-file)))

(defun score-char (val)
  (if (char< val #\a) (+ 26 (- (char-code val) 64)) (- (char-code val) 96)))

(defun intersect (l1 &rest lists)
  (if (null (CAR lists)) l1 (intersect (intersection l1 (CAR lists)) (CADR lists))))

(defmacro with-grouping-sum (grouping body)
  `(loop for group in ,grouping sum ,body))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (with-grouping-sum (mapcar (lambda (l) (list (subseq l 0 (/ (length l) 2)) (nthcdr (/ (length l) 2) l))) parsed-input)
    (apply '+ (mapcar 'score-char (remove-duplicates (apply 'intersect group))))))

;; Returns the solution for part b
(defun part-b (parsed-input) 
  (with-grouping-sum (loop for (x y z) on parsed-input by #'CDDDR collect (list x y z))
    (apply '+ (mapcar 'score-char (remove-duplicates (apply 'intersect group))))))

