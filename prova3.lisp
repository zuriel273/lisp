(defun multiplica (x)
  (cond 
    ((null x) 1)
    (t(*
       (first x) (multiplica(rest x))))))

(multiplica '(1 2 3))

(defun reduzL (l &key (i 0) (o #'multiplica))
  (cond
    ((null l) 0)
    ((eq i 0) (funcall o l))
    (t
     (let 
	 ((aux (- i 1)))     
       (reduzL (rest l) :i aux :o o)))))

(reduzL '(2 3 4))

(defun reduzProfundo2 (l &key (i 0) (o #'multiplica))
  (cond
    ((null l) 0)
    ((eq i 0) (funcall o l))
    (t
     (let 
	 ((aux (- i 1)))
       (cond
	 ((atom (first l)) (* (reduzProfundo (first l) :i 0 :o o) (reduzProfundo (rest l) :i aux :o o) )
	 (t
	  (reduzProfundo (rest l) :i aux :o o))))))))

(defun concatenarLista (L)
  (cond
    ((null L) nil)
    ((atom (first L)) (cons (first L) (concatenarLista (rest L))))
    (t (or
	(concatenarLista (first L))
	(concatenarLista (rest L))))))

(defun reduzProfundo (l &key (i 0) (o #'multiplica))
  (cond
    ((null l) 0)
    ((eq i 0) (funcall o (concatenarLista l)))
    (t
     (let 
	 ((aux (- i 1)))     
       (reduzProfundo (rest (concatenarLista l)) :i aux :o o)))))

(reduzProfundo '(2 3 (3)))


