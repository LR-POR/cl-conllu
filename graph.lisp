

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



;; testing

;; sentence token filter -> (list token)
(defun linerialize (sentence token filter)
  (let ((childs (token-childs token sentence :fn-filter filter))
	pre pos)
    (dolist (c childs)
      (if (< (token-id c) (token-id token))
	  (push c pre)
	  (push c pos)))
    (append (mappend (lambda (tk)
		       (linerialize sentence tk filter))
		     (reverse pre))
	    (list token)
	    (mappend (lambda (tk)
		       (linerialize sentence tk filter))
		     (reverse pos)))))

;; sentence token filter -> (list token)
(defun linerialize (sentence token filter &key (direction :both))
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


(defun get-phrases (sent fn-filter fn-key filter-rel &key (direction :both))
  (mapcar (lambda (atk)
	    (mapcar fn-key (linerialize sent atk filter-rel :direction direction)))
	  (remove-if-not fn-filter (sentence-tokens sent))))



(with-open-file (out "/Users/arademaker/Temp/lixo.txt" :direction :output :if-exists :supersede)
  (let* ((files (append (directory #P"~/work/cpdoc/dhbb-nlp/udp/?.conllu")
			(directory #P"~/work/cpdoc/dhbb-nlp/udp/1?.conllu"))))
    (mapcar (lambda (file)
	      (mapcar (lambda (sent)
			(format out "~%[FILE:~a ~a]~%~{N:~{~a~^ ~}~%~}"
				(pathname-name file)
				(sentence-meta-value sent "text")
				(get-phrases sent 
					     (lambda (tk)
					       (and (string= (token-upostag tk) "PROPN")
						    (string/= (token-deprel tk) "flat:name")))
					     #'token-form
					     (lambda (tk) (member (token-deprel tk)
								  '("cc" "flat:name" "det" "case" "flat")
								  :test #'equal))
					     :direction :rhs)))
		      (cl-conllu:read-conllu file)))
	    files)))






