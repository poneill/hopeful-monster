; This file contains functions specific to the transcription factor
; binding site search problem.
(load "utils.cl")
(load "evo.cl")
(defparameter non-site-proportion 1); proportion of non-sites to sites
(defparameter sites (mapcar #'string-to-list (read-file "~/hopeful-monster/data/lexa_sites.txt")))
(defparameter num-sites (length sites))
(defparameter max-site-length (apply #'max (mapcar #'length sites)))
(defparameter num-non-sites (* num-sites non-site-proportion))
(defparameter non-sites (loop for i from 0 below num-non-sites
		       collect (random-site max-site-length)))


(defun query (site pos base)
  "indicate whether site has specified base at pos"
  (let ((pos-prime (min pos (length site))))
    (equal (string (char site pos-prime)) base)))

(defparameter unary-funcs '(not))
(defparameter binary-funcs '(and or))

(defparameter problems (append sites non-sites))
(defparameter answers (append (mapcar #' lambda () 't sites)
			(mapcar #' lambda () 'nil non-sites)))

