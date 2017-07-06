
(in-package :cl-conllu)

(defun query (pattern field sentences)
  (remove-if-not
   (lambda (s)
     (some
      (lambda (tk)
	(cl-ppcre:scan pattern (slot-value field tk)))
      (cl-conllu:sentence-tokens s)))
   sentences))  

