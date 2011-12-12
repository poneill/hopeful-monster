;; This file contains utility functions whose purpose should be clear
;; from their names.  Keeping them in the project code would just be a
;; distraction.
(defvar delta '("A" "C" "G" "T"))

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