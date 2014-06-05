(defun member* (a l)
  (cond
    ((null l) nil)
    ((listp (first l))
     (or
      (member* a (first l))
      (member* a (rest l))))
    (t (or (eq (first l) a) (member* a (rest l))))))

(member* 'a '(w r 3 (2 (r t) e)))