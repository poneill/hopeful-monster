; This file contains functions specific to the transcription factor
; binding site search problem.
(load "utils.cl")
(load "evo.cl")
(defvar sites (read-file "~/hopeful-monster/data/lexa_sites.txt"))
;;(defvar max-site-length (apply #'max (mapcar #'length sites)))

(defun query (site pos base)
  "indicate whether site has specified base at pos"
  (let ((pos-prime (min pos (length site))))
    (equal (string (char site pos-prime)) base)))

(defvar unary-funcs '(not))
(defvar binary-funcs '(and or))