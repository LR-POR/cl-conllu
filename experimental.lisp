
;;; testing cl-graph for alternative representation

;; subgraph is not working!
;; dot -Tpdf documents/teste.dot  > teste.pdf

(defun sentence-graph (sentence)
  (let ((tks  (sentence-tokens sentence))
	; (g (cl-graph:make-graph 'cl-graph:graph-container))
	(g (make-graph 'dot-graph :default-edge-type :directed))
	)
    (dolist (tk tks g)
      (if (equal "root" (token-deprel tk))
	  (add-vertex g tk)
	  (add-edge-between-vertexes g (nth (- (token-head tk) 1) tks) tk
				     :edge-type :directed)))))


(defun graph-dot (filename graph)
  (with-open-file (file filename :direction :output :if-exists :supersede)
    (cl-graph:graph->dot
     graph file
     :vertex-labeler 
     (lambda (vertex stream)
       (format stream "~(~A~)" (token-form (element vertex))))
     :edge-labeler
     (lambda (edge stream)
       (format stream "~a" (token-deprel (element (slot-value edge 'vertex-1))))))))


;;; getting substrings a the sentence (flat, compounds etc)

(defun linearize (sentence token &key (filter nil) (direction :both))
  (let ((childs (token-childs token sentence :fn-filter filter))
	pre pos)
    (dolist (c childs)
      (if (< (token-id c) (token-id token))
	  (push c pre)
	  (push c pos)))
    (let ((lhs (mappend (lambda (tk)
			  (linerialize sentence tk filter :direction :both))
			(reverse pre)))
	  (rhs (mappend (lambda (tk)
			  (linerialize sentence tk filter :direction :both))
			(reverse pos))))
      (case direction
	(:both  (append lhs (list token) rhs))
	(:lhs   (append lhs (list token)))
	(:rhs   (cons token rhs))))))



(defun list-trim (deps a-list-tokens)
  (labels ((aux (alist)
	     (if (null alist)
		 alist
		 (if (member (token-deprel (car alist)) deps :test #'equal)
		     (aux (cdr alist))
		     alist))))
    (aux a-list-tokens)))


(defun get-groups (deprel sent &optional (out *standard-output*))
  (let ((leaves (remove-if-not (lambda (tk) (equal deprel (token-deprel tk)))
			       (sentence-tokens sent))))
    (mapc (lambda (alist)
	    (format out "~{~a~^ ~}~%" (mapcar #'token-form alist)))
	  (remove-duplicates (mapcar (lambda (tk)
				       (list-trim '("case" "advmod" "punct" "cc" "det")
						  (linerialize sent (token-parent tk sent) nil)))
				     leaves) :test #'equal))))

;; Execute (get-groups "compound" a-sentence)


;;; problems in the misc field

(defun fix-sentence (s)
  (dolist (alist (list (sentence-tokens s) (sentence-mtokens s)) s)
    (dolist (tk alist)
      (let ((nval (string-trim '(#\Space #\Tab #\NO-BREAK_SPACE)
			       (cl-ppcre:regex-replace "SpacesAfter=[  ]*(\\\\s|\\\\n)*[  ]*"
						       (s (null) lot-value tk 'misc) ""))))
	(if (not (equal nval (slot-value tk 'misc)))
	    (setf (slot-value tk 'misc)
		  (if (equal nval "") "_" nval)))))))

(defun fix-sentences (filename)
  (write-conllu (mapcar #'fix-sentence (read-conllu filename))
		(make-pathname :type "new" :defaults filename))) 

