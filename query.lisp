
(in-package :cl-conllu)


(defun query (pattern sentences)
  (remove-if-not (lambda (sent) (query-pattern pattern sent)) sentences))  


((cl-ppcre:scan "ficar" (slot-value 'lemma tk))
 (cl-ppcre:scan "PCP" (slot-value 'xpostag tk)))
