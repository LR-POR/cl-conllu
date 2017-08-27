(in-package :cl-conllu)


(defparameter *deprels* '(acl acl-part acl-relcl
			  advcl advmod amod
			  aux aux-pass
			  case dep det cc ccomp conj
			  appos cop csubj 
			  discourse dislocated expl
			  compound fixed flat flat-foreign flat-name
			  iobj mark
			  nmod nmod-npmod nmod-tmod nummod
			  nsubj nsubj-pass 
			  obj obl obl-agent
			  orphan parataxis
			  punct reparandum
			  xcomp vocative))


(defun children (tks id)
  (remove-if-not (lambda (tk) (equal (cl-conllu:token-head tk) id)) tks))


(defun r~ (relation query1 query2 &key tks)
  (let ((list1 (eval-query query1 :tks tks))
	(list2 (eval-query query2 :tks tks))
	(rel (string-upcase (substitute #\: #\- (symbol-name relation)))))
    (remove-if-not (lambda (tk)
		     (some (lambda (child)
			     (equal (string-upcase (token-deprel child)) rel))
			   (children list2 (cl-conllu:token-id tk))))
		   list1)))


(defun t~ (slot string &key tks)
  (remove-if-not (lambda (tk)
		   (cl-ppcre:scan string (slot-value tk slot)))
		 tks))


(defun or% (query1 query2 &key tks)
  (let ((list1 (eval-query query1 :tks tks))
	(list2 (eval-query query2 :tks tks)))
    (union list1 list2)))


(defun and% (query1 query2 &key tks)
  (let ((list1 (eval-query query1 :tks tks))
	(list2 (eval-query query2 :tks tks)))
    (intersection list1 list2)))


(defun eval-query (expression &key tks)
  (let ((op (intern (symbol-name (car expression)) :cl-conllu)))
    (cond ((member op *deprels*)
	   (destructuring-bind (deprel arg1 arg2)
	       expression
	     (declare (ignore deprel))
	     (funcall #'r~ op arg1 arg2 :tks tks)))
	  ((member op '(upostag lemma form feats misc xpostag))
	   (destructuring-bind (field value)
	       expression
	     (declare (ignore field))
	     (funcall #'t~ op value :tks tks)))
	  ((member op '(or and))
	   (destructuring-bind (op arg1 arg2)
	       expression
	     (funcall (getf '(or or% and and%) op) arg1 arg2 :tks tks)))
	  (t (error "Invalid query.")))))


(defun query (query sentences)
  (remove-if-not (lambda (s)
		   (eval-query query :tks (cl-conllu:sentence-tokens s)))
		 sentences))


(defun query-as-json (a-query sentences)
  (yason:encode (mapcar #'sentence-hash-table (query a-query sentences))))
