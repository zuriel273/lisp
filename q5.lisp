(defun substitui (a n l)
  (cond
    ((null l) (quote()))
    (t (cond
	 ((eq (first l) a) (cons n (substitui a n (rest l))))
	 (t (cons (first l)
		  (substitui a n (rest l))))))))

(substitui 3 2 '(1 2 3 3 3))

