; 1)
(defun concatenar(l1 l2)
  (cond
    ((null l1) l2)
    ((null l2) l1)
    (t (cons (first l1) (concatenar (rest l1) l2)))))  

(concatenar '(1 2 3) '(4 5 6))        


; 2)
(defun concatenar2(l1 l2)
  (cond
    ((null l1) l2)
    ((null l2) l1)
    (t (cons (first l2) (concatenar (rest l2) l1)))))  

(concatenar2 '(1 2 3) '(4 5 6))

; 3)
(defun concatenarLista (L)
  (cond
    ((null L) nil)
    ((atom (first L)) (cons (first L) (concatenarLista (rest L))))
    (t (or
	(concatenarLista (first L))
	(concatenarLista (rest L))))))

(concatenarLista '( 1 2 3 4 (4 5 6)))

(defun concatenarll(l)
	(cond
		((null l) nil)
		((atom (first l))
			(cons (first l) (concatenarll (rest l)))
		)
		((listp (first l))
			(concatenar (concatenarll (first l)) (concatenarll (rest l)))
		)
		((listp (rest l))
			(concatenar (concatenarll (first l)) (concatenarll (rest l)))
		)
		
	)
)
(concatenarll '( 1 2 3 4 (4 5 6) (3) 7))

(defun concatenarLista2 (L)
  (cond
    ((null L) nil)
    ((atom (first L)) (cons (first L) (concatenarLista2 (rest L))))
    (t 	(cons
	 (concatenarLista2 (first L))
	 (concatenarLista2 (rest L))))))

(concatenarLista2 '( 1 2 3 4 (4 5 6) 7))

;4 )
(defun intercalar(l1 l2)
  (cond
    ((null l1) l2)
    ((null l2) l1)
    (t (cons
	(first l1) (intercalar2 (rest l1) l2)))))

(defun intercalar2(a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t (cons
	(first b) (intercalar a (rest b))))))

(intercalar '(a b c) '(d))



(defun final2(a l1)
 (cond
   ((null a) l1)
   ((null l1) a)
   (t (cons
         l1 (list 'a)))))

(defun final3(a l1)
 (cons l1 (list 'a)))

(final3 2 '(a v c))

;5)
(defun final(a l1)
 (cond
   ((null a) l1)
   ((null l1) (list a))
   (t (cons
         (first l1)
	 (final a (rest l1))))))

(final 'p '(a v c))

(cons 'a '(b c))

(cons '(b c) 'a)

;6)
(defun ajuda (a b)
  (cond
    ((null a) b)
    (t (ajuda (rest a) (cons (first a) b)))))

(defun inverter (l)
  (ajuda l '()))

(inverter '(a b c))

;7)
(defun parear (a l)
  (cond
   ((null l) nil)
   (t (cons 
       (cons a (list (first l)))
       (parear a (rest l))))))

(parear 'l '(a e i o u))

;8)

(defun pares (l)
  (cond 
    ((null l) nil)
    (t (append (parear (first l) (rest l)) (pares (rest l))))))

(pares '(a b c d))

(defun pares2 (l)
  (cond 
    ((null l) nil)
    (t (concatenar (parear (first l) (rest l)) (pares2 (rest l))))))

(pares2 '(a b c d))

; 9)
(defun permutar (lst &optional (aux lst))
  (cond ((null aux) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (permutar (rest lst)))
            (permutar (append (rest lst) (list (first lst))) (rest aux))))))


  
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

(defun membro (a l)
  (cond
    ((null l) nil)
    ((null a) nil)
    ((eq (first l) a) t)
    (t (membro a (rest l)))))

;10)
(defun conjunto (l)
  (cond 
    ((null l) t)
    ((membro (first l) (remove6 (first l) l)) nil)
    (t (conjunto (rest l)))))

(conjunto '(a b c d e))

;11)
(defun prefixo(l1 l2)
  (cond
     ((null l1) t)
     ((null l2) t)
     ((eq (first l1) (first l2)) (prefixo (rest l1) (rest l2)))))

(prefixo '(a b c) '(a b d e))

;12)
(defun subsequencia (l1 l2)
  (cond
     ((null l1) nil)
     ((null l2) nil)
     ((prefixo l1 l2) t)
     (t 
      (subsequencia l1 (rest l2)))))

(subsequencia '(a b c) '(d z a b c f g))

