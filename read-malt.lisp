;; https://common-lisp.net/project/xmls/

(ql:quickload :cxml)
(ql:quickload :cxml-stp)
(ql:quickload :xpath)

(defun read-sentences (node)
  (destructuring-bind (name attributes nodes)
      node
    (cond ((equal name "treebank")
	   (mapcar #'read-sentences nodes))
	  ((equal name "body")
	   (mapcar #'read-sentences nodes))
	  ((equal name "sentence")))))

(defun read-malt (filename)
  (let ((doc (cxml:parse filename (stp:make-builder))))
    (mapcar (lambda (node)
		   (list (cxml-stp:attribute-value node "form")
			 (cxml-stp:attribute-value node "base")
			 (cxml-stp:attribute-value node "postag")))
	    (xpath:all-nodes (xpath:evaluate "//word[@postag='prop' or @postag='n']" doc)))))

(defun compare (list-a dict)
  (let ((data (mapcar (lambda (node) (destructuring-bind (a b c)
					 node
				       (list (substitute #\Space #\_ a) b c)))
		      list-a)))
    (remove-if (lambda ()) data)))

