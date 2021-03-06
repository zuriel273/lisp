(defun foo (a b &optional c d) (list a b c d))

(foo 1 2 3 4)

(defun lista(&rest numeros)
  numeros)

(lista 'a 'b 'c 'd 'e 'f)

(defun twice (num)
  (* num 2))

(mapcar #'twice '(1 2 3 4))

(mapcar #'+ '(1 2 3 4) '(1 2 3 6))

(find 4 '(1 2 3 4 5))

(find '(A B)'((A B) (C D) (E F)) :test #'equal)

(find 4 '((A B) (C D E) (F G H I)) :key #'length) 

(find '(d e) '((a b) (c d e) (f g h i)) :key #'rest :test #'equal)

(find-if #'evenp '(1 2 3 4))

(find-if #'oddp '((0 1) (1 2) (2 3)):key #'first)

(remove 1 '((0 1) (1 2) (2 3)) :key #'first)

(defun qs (l &optional (f #'<))
  (cond
    ((null l) nil)
    ((null (cdr l)) l)
    (t 
     (append
      (qs (remove-if-not
	   (lambda (x) (funcall f x (car l)))
	   (cdr l)) f)
      (list (car l))
      (qs (remove-if
	   (lambda (x) (funcall f x (car l)))
	   (cdr l)) f)
      ))))

(qs '(3 4 5 6) #'<)
(qs '(3 4 5 6) #'>)
(qs '(3 4 5 6) #'=)


(defun addvetor (vec)
  (cond
    ((null vec) 0)
    (t (+ 
	(first vec) 
	(addvetor (rest vec))))))

(defun  idmane(x) x)

(defun find3 (a l &key (test #'eq) (chave #'idmane))
  (cond
    ((null l) nil)
    (( funcall test a ( funcall chave (first l))) (first l))
    (t (find3 a (cdr l) :test test :chave chave))))

(find3 '2 '((d 2) (s 1) (a 3)) :test #'> :chave #'cadr)

