; This file contains functions specific to the transcription factor
; binding site search problem.
(load "utils.cl")

(defparameter non-site-proportion 1); proportion of non-sites to sites
(defparameter sites (mapcar #'string-to-list (read-file "~/hopeful-monster/data/lexa_sites.txt")))
(defparameter num-sites (length sites))
(defparameter max-site-length (apply #'max (mapcar #'length sites)))
(defparameter num-non-sites (* num-sites non-site-proportion))
(defparameter non-sites (loop for i from 0 below num-non-sites
		       collect (random-site max-site-length)))

(defparameter numeric-constants 
  (loop for i from 0 below max-site-length collect i))
(defparameter constants (append '(t nil) numeric-constants delta))
(defparameter variables (list 'input))
(defparameter nullary-funcs '(base))
(defparameter unary-funcs '(not))
(defparameter binary-funcs '(and or))
(defparameter numeric-types (mapcar (lambda (i) (list i '(num))) 
				    numeric-constants))

(defparameter types (append '((t (bool))
			      (nil (bool))
			      (not (bool bool))
			      (and (bool bool bool))
			      (or (bool bool bool))
			      (equal (base base bool))
			      (query (site num base))
			      (input (site))
			      (+ (num num num))
			      (* (num num num))
			      (- (num num num))
			      (A (base))
			      (C (base))
			      (G (base))
			      (Th (base))
			      )
			    numeric-types))

(defun get-input ()
  input)

(defun A () "A")
(defun C () "C")
(defun G () "G")
(defun Th () "T")

(defun query (input pos)
  "indicate whether site has specified base at pos"
  (let ((pos-prime (max 0 (min pos (length input)))))
    (nth pos-prime input)))

(defparameter unary-funcs '(not))
(defparameter binary-funcs '(and or))

(defparameter problems (append sites non-sites))
(defparameter answers (append (mapcar (lambda (x) 't) sites)
			(mapcar #' (lambda (x) 'nil) non-sites)))
(defparameter answer-type 'bool)

(defun fitness (p)
  (print "calling fitness")
  (handler-case 
      (let* ((responses (mapcar (lambda (x) (evaluate p x)) problems))
	     (fit (sum (zipwith (lambda (x y) (if (equal x y) 1 0))
				responses answers))))
	(if (and (realp fit) (< fit fitness-penalty))
	    fit
	    fitness-penalty))
    (error (e) fitness-penalty)))