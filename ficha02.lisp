;;;;
;;;; IIA 2004/2005
;;;;
;;;; Ficha 02 : Programa��o em Common Lisp
;;;;
;;;; - Turma Pr�tica: p4
;;;; - N�mero do Grupo: 3
;;;;
;;;; - Nome 1: Jo�o Andr� Pinto de S� Borges
;;;; - N�mero: 501021230
;;;; - Nome 2: Tiago Xavier Gomes
;;;; - N�mero: 985011466

;;;
;;; Opera��es sobre Listas
;;;

;;
;; Exerc�cio 1

;;(a)
(cons 'a '())

;;(a b c)
(cons 'a '(b c))

;;(1 (2 3))
(cons 1 (cons (cons 2 '(3)) '()))

;;((1 2) 3)
(cons (cons 1 '(2)) (cons 3 '()))

;;(() 3)
(cons '() '(3)) ;;????????????????????????????

;;
;; Exerc�cio 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(resto2 <lista>)
(defun resto2 (l)
  (rest (rest l)))

(resto2 '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(terceiro <lista>)
(defun terceiro (l)
  (first (rest (rest l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(primeiro-se-lista <lista>)
(defun primeiro-se-lista (l)
  (if (listp (first  l)) (first l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(soma-primeiro-segundo <lista>)
(defun soma-primeiro-segundo (l)
  (if (and (numberp (first  l)) (numberp (first (rest l)))) (+ (first l) (first (rest l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(obtem-restos <lista1> <lista2>)
(defun obtem-restos (l1 l2)
  (list (rest l1) (rest l2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(lista-comprimentos <lista1> <lista2> <lista3>)
(defun lista-comprimentos (l1 l2 l3)
  (list (if (listp l1) (length l1) -1) (if (listp l2) (length l2) -1) (if (listp l3) (length l3) -1)))


;;
;; Exerc�cio 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(multiplica <lista-de-numeros>)
(defun multiplica (l)
  (if (> (length l) 0)
  (* (first l) (multiplica (rest l))) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(multiplica-numero <numero> <lista-de-numeros>)
(defun multiplica-numero (n l)
  (if (not (null l))
  (cons (* n (first l)) (multiplica-numero n (rest l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(membro <elemento> <lista>)
(defun membro (e l)
  (if (> (length l) 0)
  (if (eq e (first l)) t (membro e (rest l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(junta <lista1> <lista2>)
(defun junta (l1 l2)
  (if (or (> (length l1) 0) (> (length l2) 0))
  (cons (if (> (length l1) 0)(first l1) (first l2)) (if (> (length l1) 0)(junta (rest l1) l2) (junta l1 (rest l2))))))

� (une <lista1> <lista2>)
Devolve a uni~ao das duas listas (conjuntos).
� (intersecta <lista1> <lista2>)
Devolve a intersec�c~ao das duas listas (conjuntos).
� (diferenca <lista1> <lista2>)
Devolve a diferen�ca das duas listas (conjuntos).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(inverte <lista>)
Inverte a ordem dos elementos de lista.
(defun inverte (l)
  (if (not (null l))
  (append (inverte (rest l)) (list (first l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(aplana <lista>)
Devolve uma lista com apenas os elementos at�micos de lista.
(defun aplana (l)
   (if (not (null l))
  (append (if (listp (first l)) (aplana (first l)) (list (first l))) (aplana (rest l)))))

� (profundidade <lista>)
Recebe uma lista de listas e devolve o n��vel de profundidade.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(elimina <elemento> <lista>)
(defun elimina (e l)
  (if (not (null l))
  (if (not (eq e (first l))) (cons (first l) (elimina e (rest l))) (elimina e (rest l)))))

� (ocurrencias <lista>)
Devolve uma lista de pares ordenados onde cada par cont�em um elemento
de lista e o respectivo n�umero de vezes que aparece.
� (expande-pares <lista>)

Recebe uma lista de pares ordenados onde cada par cont�em um elemento e
o n�umero de repeti�c~oes. Devolve uma lista simples com os elementos dos
pares ordenados, repetidos o n�umero de vezes indicado.


;;;
;;; Tipos de Dados Abstractos
;;;

;;
;; Exerc�cio 1

;; Pilha

; construtores
(defun new-pilha ()
	'())

(defun push-pilha (x pilha)
	(cons x pilha))

(defun remove-end-pilha (pilha)
	(if (= (length ) 1) nil
	(cons (first pilha) (remove-end-pilha (rest pilha)))))

; destrutores
(defun pop-pilha (pilha)
	(rest pilha))

(defun clear-pilha (pilha)
	(null pilha))

; selectores
(defun top-pilha (pilha)
	(first pilha))

(defun last-pilha (pilha)
	(if (= (length pilha) 1)
		(first pilha)
	(last-pilha (rest pilha))))

; predicados



;; Fila

; construtores
(defun new-fila ()
	'())

(defun add-fila (x fila)
	(if (null fila)
	(cons x ())
	(cons (first fila) (add-fila x (rest fila)))))


(defun clear-fila (fila)
	(null fila))

; destrutores


(defun remove-fila (fila)
	(rest fila))

(defun remove-end-fila (fila)
	(if (= (length fila) 1) nil
	(cons (first fila) (remove-end-fila (rest fila)))))


; selectores
(defun first-fila (fila)
	(first fila))

(defun last-fila (fila)
	(if (= (length fila) 1)
		(first fila)
	(last-fila (rest fila))))
; predicados


;;
;; Exerc�cio 2

;; Pilha



;; Fila


;;;
;;; Algoritmos de Ordenamento
;;;

;;
;; Bubble Sort
(defun bs (l)
(if (ord l) l
	(if (= (length l) 1)
		l
(if (> (first l) (first (rest l))) (cons (first (rest l)) (bs (cons (first l) (rest (rest l))))) (cons (first l) (bs (rest l)))))))

(defun ord (l)
(if (= (length l) 1)
		t
		(if (> (first l) (first (rest l)))
		nil
		(ord (rest l))
		)))

(defun bubble (l)
(if (ord l)
	l
	(bubble (bs l))))

;;
;; Merge Sort



;;;
;;; Torres de Hanoi
;;;
(defun hanoi (num t1 t2 t3)
	(if (= num 1) (format t "move de ~A para ~A~%" t1 t3))
	(unless (= num 1) 
	(hanoi (- num 1) t1 t3 t2)
	(format t "move de ~A para ~A~%" t1 t3)
	(hanoi (- num 1) t2 t1 t3)
	)
	)


;;;
;;; Automatos Finitos
;;;

(defun f1 (op)
	(cond
	((= op 0) 1)
	((= op 1) 2)))

(defun f2 (op)
	(cond
	((= op 0) 1)
	((= op 1) 2)))

(defun myadf (st l)
(if(not (null (rest l)))
	(cond
	((= st 1) (myADF (f1 (first l)) (rest l)))
	((= st 2) (myADF (f2 (first l)) (rest l))))
	(cond
	((= st 1) (f1 (first l))
	((= st 2) (f2 (first l)))))))
