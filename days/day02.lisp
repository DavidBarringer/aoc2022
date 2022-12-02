;; Put the given solutions for the examples here
(setf test-sol-a 15)
(setf test-sol-b 12)

(defun parse-input (input-file)
  "Remove the spaces from each line, then turn into a variable name
   this is a surprise tool to help us later"
  (mapcar (lambda (s) (intern (remove #\  s))) (get-file-lines input-file)))

(defmacro with-scores (scores body)
  "Makes a scope with each result assigned to its score, 
   declare special allows these to be accessed with symbol-value
   then evaluate the body"
  `(let ,scores (declare ,(cons 'special (mapcar 'CAR scores))) ,body))

;; Returns the soluton for part a
(defun part-a (parsed-input)
  (with-scores ((AX 4) (AY 8) (AZ 3) (BX 1) (BY 5) (BZ 9) (CX 7) (CY 2) (CZ 6))
    (apply '+ (mapcar 'symbol-value parsed-input))))


;; Returns the solution for part b
(defun part-b (parsed-input)
  (with-scores ((AX 3) (AY 4) (AZ 8) (BX 1) (BY 5) (BZ 9) (CX 2) (CY 6) (CZ 7))
    (apply '+ (mapcar 'symbol-value parsed-input))))
