(defun remova (a l)
  (cond 
    ((null l) nil)
    ((eq (first l) a) (rest l))
    (t (cons (first l) (remova a (rest l))))))

(remova 'a '(b c a f e a))

(defun remova2 (a l)
  (cond
    ((null l) nil)
    ((atom (first l)) 
     (cond
       ((eq (first l) a) (rest l))
       (t (cons (first l) (remova2 a (rest l))))))
    (t (cons
	(remova2 a (first l))
	(remova2 a (rest l))))))
     
(remova2 'a '(b c f e (a 2 3) a))	  

(defun menor (l &optional aux)
  (cond 
    ((null l) aux)
    ((null aux) (menor (rest l) (first l)))
    ((< (first l) aux) (menor (rest l) (first l)))
    (t (menor (rest l) aux))))

(menor '(4 2 5 1 6))

(defun bubbleSort (l)
  (cond 
    ((null l) nil)
    (t (cons (menor l) (bubbleSort (remova (menor l) l))))))

(bubbleSort '(4 2 5 1 6))

(defun idhelp (x) x)

(idhelp '4)

(defun remova3 (a l &optional (f #'eq))
  (cond 
    ((null l) nil)
    ((funcall f (first l) a) (rest l))
    (t (cons (first l) (remova3 a (rest l) f)))))

(remova3 'a '(b c f e a 2 3 a) #'eq)
(remova3 1 '(2 3 4 5 6 0 -1 2 3) #'>)

(defun remova4 (a l &key (f #'eq) (k #'idhelp))
  (cond 
    ((null l) nil)
    ((funcall f a (funcall k (first l)))(rest l))
    (t (cons (first l) (remova4 a (rest l) :f f :k k)))))

(remova4 'a '(b c a f e a))



	  
	  