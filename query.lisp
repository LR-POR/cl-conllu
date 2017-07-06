(in-package :cl-conllu)

(defun query (pattern field sentences)
  (remove-if-not
   (lambda (s)
     (some (lambda (tk)
	     (cl-ppcre:scan pattern (slot-value tk (intern (string-upcase field)
							   :cl-conllu))))
	   (cl-conllu:sentence-tokens s)))
   sentences))  

