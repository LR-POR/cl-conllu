(in-package :cl-conllu)

;; VERB <advcl L=correr&VERB
;; L=correr&VERB >advcl VERB

;; (nsubj (advcl (and (upostag ~ "VERB")
;; 		   (lemma ~ "correr"))
;; 	      (upostag ~ "VERB"))
;;        (upostag ~ "PROP"))


(defmacro match (field pattern)
  `(lambda (tokens)
     (remove-if-not (lambda (tk)
		      (cl-ppcre:scan ,pattern
				     (slot-value tk (intern (string-upcase ,field) :cl-conllu))))
		    tokens)))



(defun query (pattern field sentences)
  (remove-if-not
   (lambda (s)
     (some (lambda (tk)
	     (cl-ppcre:scan pattern (slot-value tk (intern (string-upcase field)
							   :cl-conllu))))
	   (cl-conllu:sentence-tokens s)))
   sentences))

(defun query-as-json (pattern field sentences)
  (yason:encode (mapcar #'sentence-hash-table (query pattern field sentences))))

