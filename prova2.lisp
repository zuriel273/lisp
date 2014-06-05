(defun soma (x)
  (cond 
    ((null x) 0)
    (t(+
       (first x) (soma(rest x))))))

(soma '(1 2 3 4))

(defun reduzL (l &key (i 0) (o #'soma))
  (cond
    ((null l) 0)
    ((eq i 0) (funcall o l))
    (t
     (let 
	 ((aux (- i 1)))     
       (reduzL (rest l) :i aux :o o)))))

(reduzL '(1 2 3 4 5) :i 5 :o #'soma)


(defun ajuda (a b)
  (cond
    ((null a) b)
    (t (ajuda (rest a) (cons (first a) b)))))

(defun inverter (l)
  (ajuda l '()))

;solução pensando que ele qr fazer a "soma" da direita para esquerda 
(defun reduzD (l &key (i 0) (o #'soma))
   (reduzL (inverter l) :i i :o o))


(reduzD '(1 2 3 4 5) :i 4 :o #'soma)

       
; pensamento de java

(defun reduzD2 (l &key (i 0) (o #'soma))
  (cond
    ((null l) 0)
    ((eq i 0) (funcall o (inverter l)))
    (t
     (let 
	 ((aux (- i 1)))     
       (reduzD2 (rest l) :i aux :o o)))))

(reduzD2 '(1 2 3 4 5) :i 4 :o #'soma)

; b) no caso da subtração a ordem(divisão) importa.
     


