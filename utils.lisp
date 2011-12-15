;; This file contains utility functions whose purpose should be clear
;; from their names.  Keeping them in the project code would just be a
;; distraction.
(defpackage :utils
  (:use :cl))
(in-package :utils)

(defparameter delta '("A" "C" "G" "T"))

(defun read-file (file-name)
  (with-open-file (stream file-name)
    (loop for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       collect line)))

(defun range (start stop) ;Thanks to Rob Warnock on comp.lang.lisp
  (loop for i from start below stop collect i))

(defun random-site (len)
  (loop for i from 0 below len collect (choose-randomly delta)))

(defun choose-randomly (lst)
  (nth (random (length lst)) lst))

(defun string-to-list (str)
  (mapcar #'string (loop for x being the elements of str collect x)));damn
								     ;loop,
								     ;you
								     ;scary.

(defun normalize (xs)
  (mapcar (lambda (x)(/ x (sum xs))) xs))

(defun discrete-cmf (pmf &optional (acc 0))
  "Return the cdf of a pmf"
  (if pmf
      (let ((acc-prime (+ (car pmf) acc)))
	(cons acc-prime (discrete-cmf (cdr pmf) acc-prime)))))

(defun mean (xs)
  (/ (sum xs) (length xs)))

(defun safe-mean (xs &optional (acc 0) (n 0)); sometimes we need to compute very large means
  (if xs
      (let* ((x (car xs))
	     (rest (cdr xs))
	     (acc-prime (/ (+ (* acc n) x) (+ n 1))))
	(print n)
	(safe-mean rest acc-prime (+ n 1)))
      acc))

(defun iterate-function (f n args)
  (if (= n 1)
      (funcall f args)
      (iterate-function f (- n 1) (funcall f args))))

(defun zip (xs ys)
  (if (or (null xs) (null ys))
      nil
      (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))				  
(defun zipwith (f xs ys)
  (if (or (null xs) (null ys))
      ()
      (cons (funcall f (car xs) (car ys))
	    (zipwith f (cdr xs) (cdr ys)))))

(defun depth (tree)
  (if (atom tree)
      0
      (+ 1 (apply #'max (mapcar #'depth tree)))))

(defun size (tree)
  (if (atom tree)
      1
      (+ (length tree) (sum (mapcar #'depth tree)))))

(defun variance (xs)
  (- (mean (mapcar #'square xs)) (square (mean xs))))

(defun sum (xs)
  (apply #'+ xs))

(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(defun square (x)
  (* x x))

(defun lookup (a tab)
  (if tab
      (let ((pair (first tab)))
	(if (equal a (first pair))
	    (second pair)
	    (lookup a (rest tab))))
      nil))

(defun head (x)
  "safely take the first element, returning self if not a list"
  (if (listp x)
      (first x)
      x))

(defun shush (x)
  "Suppress output"
)

(defun separate (f xs &optional (sheep nil) (goats nil))
  (if (null xs) 
      (list sheep goats)
      (let ((x (first xs)))
	(if (funcall f x)
	    (separate f (rest xs) (cons x sheep) goats)
	    (separate f (rest xs) sheep (cons x goats))))))
      
(defun qsort (xs)
  (if (< (length xs) 2)
      xs
      (let ((x (first xs))
	    (rests (rest xs)))
	(append (qsort (remove-if (lambda (y) (> y x)) rests)) 
		(list x)
		(qsort (remove-if (lambda (y) (<= y x)) rests))))))

(defun argmin (xs f &optional (curr nil) (fcurr nil))
  (cond ((null xs) curr)
	((null fcurr) (argmin (rest xs) f (first xs) (funcall f (first xs))))
	(t (let* ((x (first xs))
		  (fx (funcall f x)))
	     (if (< fx fcurr)
		 (argmin (rest xs) f x    fx)
		 (argmin (rest xs) f curr fcurr))))))

(defun tee (f xs)
  (let ((out (apply f xs)))
    (print out)
    xs))

(defun prune (p)
  (if (atom p)
      p
      (let ((operator (first p))
	    (operands (mapcar #'prune (rest p))))
	(cond ((equal operator 'and) 
	       (cond ((some #'null operands) nil)
		     (t (let ((operands-prime 
			       (unique (remove-if #'true operands))))
			  (cond ((= (length operands-prime) 0) t)
				((= (length operands-prime) 1) (first operands-prime))
				(t (cons 'and operands-prime)))))))
	      ((equal operator 'or) 
	       (cond ((some #'true operands) t)
		     (t (let ((operands-prime (unique (remove-if #'null operands))))
			  (cond ((= (length operands-prime) 0) nil)
				((= (length operands-prime) 1) (first operands-prime))
				(t (cons 'or operands-prime)))))))
	      ((equal operator 'not)
	       (let ((operand (first operands)))
		 (cond ((equal operand 'nil) t)
		       ((equal operand t) nil)
		       ((equal (first operand) 'not) (second operand))
		       (t (cons 'not operands)))))
	      (t p)))))

(defun true (x) 
  (equal t x))

(defun fold (f x xs)
  (if (null xs)
      x
      (fold f (funcall f x (first xs)) (rest xs))))

(defun fold1 (f xs)
  (fold f (first xs) (rest xs)))

(defun unique (ys)
  (reverse (fold (lambda (xs x) (if (member x xs) xs (cons x xs))) '() ys)))