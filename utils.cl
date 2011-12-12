;; This file contains utility functions whose purpose should be clear
;; from their names.  Keeping them in the project code would just be a
;; distraction.

(defun read-file (file-name)
  (with-open-file (stream file-name)
    (loop for line = (read-line stream nil 'eof)
       until (eq line 'eof)
       collect line)))