
(ql:quickload :cl-conllu)
(ql:quickload :cl-graph)

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

;; sentence token -> (list token)
(defun linerialize (sentence token)
  (let ((childs (token-childs token sentence))
	pre pos)
    (dolist (c childs)
      (if (< (token-id c) (token-id token))
	  (push c pre)
	  (push c pos)))
    (append (mappend (lambda (tk)
		      (linerialize sentence tk))
		     (reverse pre))
	    (list token)
	    (mappend (lambda (tk)
		       (linerialize sentence tk))
		     (reverse pos)))))


(defun get-phrases (sent fn-filter fn-key)
  (mapcar (lambda (atk)
	    (mapcar fn-key (linerialize sent atk)))
	  (remove-if-not fn-filter (sentence-tokens sent))))


(mapcar (lambda (sent)
	  (format t "[~a]~%~{~{~a~^ ~}~%~}~%"
		  (sentence-meta-value sent "text")
		  (get-phrases sent 
			       (lambda (tk) (string= (token-upostag tk) "NOUN"))
			       #'token-upostag)))
	(read-conllu #P"CF107.conllu"))

