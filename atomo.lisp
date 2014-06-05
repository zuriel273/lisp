(defun non-atom (a)
  (cond
    ((atom a) nil)
    (t t))
)



(non-atom '(a f))
