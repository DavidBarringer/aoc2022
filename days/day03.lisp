;; Put the given solutions for the examples here
(setf test-sol-a 157)
(setf test-sol-b 70)

(defun parse-input (input-file)
  "Turns each line into a list of characters"
  (mapcar (lambda (s) (concatenate 'list s)) (get-file-lines input-file)))

(defun score-char (val)
  (if (char< val #\a) (+ 26 (- (char-code val) 64)) (- (char-code val) 96)))

(defun intersect (l1 &rest lists)
  "Function to calculate the intersection of
   multiple lists"
  (if (null (CAR lists)) l1 (intersect (intersection l1 (CAR lists)) (CADR lists))))

(defmacro with-grouping-sum (grouping body)
  "Takes a list of 'groups' and applies the body to each
   group, then sums the result of each"
  `(loop for group in ,grouping sum ,body))

(defun part-a (parsed-input)
  (with-grouping-sum (mapcar (lambda (l) (list (subseq l 0 (/ (length l) 2)) (nthcdr (/ (length l) 2) l))) parsed-input)
    (apply '+ (mapcar 'score-char (remove-duplicates (apply 'intersect group))))))

(defun part-b (parsed-input) 
  (with-grouping-sum (loop for (x y z) on parsed-input by #'CDDDR collect (list x y z))
    (apply '+ (mapcar 'score-char (remove-duplicates (apply 'intersect group))))))

