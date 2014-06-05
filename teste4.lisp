(defun rmember (a l)
  (cond
    ((null l) (quote ()))
    (t (cond
	 ((eq (first l) a) (rest l))
	 (t (cons (first l)
		  (rmember
		   a (rest l))))))))

(rmember 'a '(1 a a 3 4))


