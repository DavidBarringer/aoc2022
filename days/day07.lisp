;; Put the given solutions for the examples here
(setf test-sol-a 95437)
(setf test-sol-b 24933642)

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (s) (split #\  s)) (get-file-lines input-file)))

(defstruct file parent name size children)

(defun add-child (parent cname child)
  (setf (file-children parent) (cons (cons cname child) (file-children parent))))

(defun find-child (parent child)
  (loop for (s . cname) in (file-children parent)
	if (string= s child)
	  return cname))

(defun fold-up-and-return (curr)
  (cond ((null (file-parent curr)) curr)
	(t (inc-file-size (file-parent curr) curr)
	   (add-to-parents (file-parent curr)))))

(defun build-fs (parent curr io)
  (cond ((null io) (fold-up-and-return curr))
	((string= "$" (CAAR io))
	 (cond ((string= "cd" (CADAR io))
		(cond ((string= ".." (CADDAR io))
		       (incf (file-size parent) (file-size curr))
		       (build-fs (file-parent parent) parent (CDR io)))
		      (t (build-fs curr (find-child curr (CADDAR io)) (CDR io)))))
	       ((string= "ls" (CADAR io)) (build-fs parent curr (CDR io)))))
	((string= "dir" (CAAR io))
	 (let ((name (make-file :parent curr :size 0 :children nil)))
	   (add-child curr (CADAR io) name)
	   (build-fs parent curr (CDR io))))
	(t (let ((name (make-file :parent curr :size (parse-integer (CAAR io)) :children nil)))
	     (add-child curr (CADAR io) name)
	     (incf (file-size curr) (file-size name))
	     (build-fs parent curr (CDR io))))))

(defun find-dirs (curr check)
  (let ((result (loop for (s . child) in (file-children curr) nconc (find-dirs child check))))
    (if (funcall check curr) (cons curr result) result)))
      
;; Returns the solution for part a
(defun part-a (parsed-input)
  (apply '+ (mapcar 'file-size (find-dirs 
				(build-fs nil (make-file :parent nil :size 0 :children nil) (CDR parsed-input))
				(lambda (x) (AND (NOT (null (file-children x)))
						 (<= (file-size x) 100000)))))))

;; Returns the solution for part b
(defun part-b (parsed-input)
  (apply 'min (mapcar 'file-size (find-dirs 
				  (build-fs nil (make-file :parent nil :size 0 :children nil) (CDR parsed-input))
				  (lambda (x) (AND (NOT (null (file-children x)))
						   (> 40000000 (- (file-size root) (file-size x)))))))))
