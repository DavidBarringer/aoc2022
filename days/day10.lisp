;; Put the given solutions for the examples here
(setf test-sol-a 13140)
(setf test-sol-b (format nil (remove #\return "
██  ██  ██  ██  ██  ██  ██  ██  ██  ██  
███   ███   ███   ███   ███   ███   ███ 
████    ████    ████    ████    ████    
█████     █████     █████     █████     
██████      ██████      ██████      ████
███████       ███████       ███████     ")))

;; Turn the input file into whatever form you will use for both parts
;; (get-file-lines) and (get-file-string) will be useful
(defun parse-input (input-file)
  (mapcar (lambda (l) (if (null (CDR l)) (intern (CAR l)) (cons (intern (CAR l)) (parse-integer (CADR l)))))
	  (mapcar (lambda (s) (split #\  s)) (get-file-lines input-file))))

;; Returns the solution for part a
(defun part-a (parsed-input)
  (let ((x 1))
    (apply '+ (loop with i = 1
	  for inst in parsed-input
	  do (incf i 1)
	  if (find i '(20 60 100 140 180 220))
	    collect (* i x)
	  if (listp inst)
	    do (incf i 1)
	       (incf x (CDR inst))
	    and 
	      if (find i '(20 60 100 140 180 220))
		collect (* i x)))))

	     

;; Returns the solution for part b
(defun part-b (parsed-input)
  (let ((x 1))
    (format nil "~%~{~A~}"
	    (loop with i = 0
		  for inst in parsed-input
		  do (incf i 1)
		  if (= (mod i 41) 0)
		    do (setf i 1)
		    and collect #\newline
		  collect (if (find (- i 1) (list (- x 1) x (+ x 1))) #\█ #\ )
		  if (listp inst)
		    do (incf i 1)
		    and when (= (mod i 41) 0)
			  do (setf i 1)
			  and collect #\newline
		  end
		  and collect
		      (if (find (- i 1) (list (- x 1) x (+ x 1))) #\█ #\ )
		  and do (incf x (CDR inst))))))

