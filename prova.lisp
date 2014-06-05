
(defun soma (l)
  (cond
    ((null l) 0)
    (t ( +
	 (first l)
	 (soma (rest l))))))

(soma '(1 2 3 5 56))

