;; Put the given solutions for the examples here
(setf test-sol-a 21)
(setf test-sol-b "No test solution")

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (let ((input (mapcar (lambda (s) (mapcar 'digit-char-p (concatenate 'list s))) (get-file-lines input-file))))
    (make-array (list (length input) (length (CAR input))) :initial-contents input)))

(defun visible (arr x y)
  (notevery 'null (list
		   (notany 'null (loop for r1 from 0 below x collect (< (aref arr r1 y) (aref arr x y))))
		   (notany 'null (loop for r2 from (+ x 1) below (CAR (array-dimensions arr)) collect (< (aref arr r2 y) (aref arr x y))))
		   (notany 'null (loop for c1 from 0 below y collect (< (aref arr x c1) (aref arr x y))))
		   (notany 'null (loop for c2 from (+ y 1) below (CADR (array-dimensions arr)) collect (< (aref arr x c2) (aref arr x y)))))))

(defun scenic (arr x y)
  (* (loop for r1 from (- x 1) downto 0 count 1 until (>= (aref arr r1 y) (aref arr x y)))
     (loop for r2 from (+ x 1) below (CAR (array-dimensions arr)) count 1 until (>= (aref arr r2 y) (aref arr x y)))
     (loop for c1 from (- y 1) downto 0 count 1 until (>= (aref arr x c1) (aref arr x y)))
     (loop for c2 from (+ y 1) below (CADR (array-dimensions arr)) count 1 until (>= (aref arr x c2) (aref arr x y)))))


;; Returns the solution for part a
(defun part-a (parsed-input)
  (+ (- (apply '* (array-dimensions parsed-input)) (apply '* (mapcar (lambda (d) (- d 2)) (array-dimensions parsed-input))))
     (loop for x from 1 below (- (CAR (array-dimensions parsed-input)) 1)
	   sum (loop for y from 1 below (- (CADR (array-dimensions parsed-input)) 1)
		     count (visible parsed-input x y)))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (loop for x from 1 below (- (CAR (array-dimensions parsed-input)) 1)
	maximize (loop for y from 1 below (- (CADR (array-dimensions parsed-input)) 1)
		  maximize (scenic parsed-input x y))))
