(in-package :cl-conllu)

;; VERB <advcl L=correr&VERB
;; L=correr&VERB >advcl VERB

;; (nsubj (advcl (and (upostag ~ "VERB")
;; 		   (lemma ~ "correr"))
;; 	      (upostag ~ "VERB"))
;;        (upostag ~ "PROP"))
;; -> (lambda ...)

;; (defmacro match (field pattern)
;;   `(lambda (tokens)
;;      (remove-if-not (lambda (tk)
;; 		      (cl-ppcre:scan ,pattern
;; 				     (slot-value tk (intern (string-upcase ,field) :cl-conllu))))
;; 		    tokens)))

;; (execute '(nsubj (advcl (and (upostag ~ "VERB")
;; 			     (lemma ~ "correr"))
;;   		        (upostag ~ "VERB"))
;; 	         (upostag ~ "PROP"))
;; 	 tokens)


;; (execute '(r~ nsubj (r~ advcl (%or (~ upostag "VERB")
;; 			           (~ lemma "correr"))
;; 	  	              (~ upostag "VERB"))
;; 	            (~ upostag "PROP"))
;; 	 tokens)


(defun compile-query (expression)
  "...")


(defun query (query sentences)
  (remove-if-not (lambda (s)
		   (some (compile-query query) 
			 (cl-conllu:sentence-tokens s)))
		 sentences))


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

