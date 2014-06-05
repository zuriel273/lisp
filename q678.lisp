
(defun remove6 (a l)
  (cond 
    ((null l) nil)
    ((atom (first l))
     (cond
       ((eq (first l) a) (rest l))
       (t (cons (first l) (remove6 a (rest l))))))
    (t (or
	(cons (remove6 a (first l)) (rest l))
	(cons (first l) (remove6 a (rest l)))
	))))

(defun remove7 (a l)
  (cond 
    ((null l) (quote()))
    ((atom (first l))
     (cond
       ((eq (first l) a) (remove7 a (rest l)))
       (t (cons (first l) (remove7 a (rest l))))))
    (t (or
	(cons (remove7 a (first l)) (rest l))
	(cons (first l) (remove7 a (rest l)))
	))))

(remove7 'a '(a (a a 4 a) 4))
(remove6 'a '(a (a a 4 a) 4))

(defun inverte (lista temp)
	(cond
		((null lista) temp)
		(T (inverte (cdr lista) (cons (car lista) temp)))
	)
)
(defun inverteLista (lista)
	(inverte lista '())
)

(inverteLista '((a b) c b a c))