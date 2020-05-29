
(in-package :cl-conllu)

#|

Verifies if a sentence tree is projective. Intuitively, this means
that, keeping word order, there's no two dependency arcs that cross.
 
More formally, let i -> j mean an arc where j's head is node i. Let
'->*' be the transitive closure of '->'. An arc is projective if for
each node k between i and j (i < k < j or j < k < i), i ->* k.

A tree is projective when all its arcs are projective. 

References:

@book{kübler2009dependency,
  title={Dependency Parsing},
  author={K{\"u}bler, S. and McDonald, R. and Nivre, J.},
  isbn={9781598295962},
  series={Synthesis lectures on human language technologies},
  url={https://books.google.com.br/books?id=k3iiup7HB9UC},
  year={2009},
  publisher={Morgan \& Claypool}
}

and

- Nivre, Joakim; Inductive Dependency Parsing, 2006
- https://en.wikipedia.org/wiki/Discontinuity_(linguistics)
- https://www.aclweb.org/anthology/E06-1010.pdf

|#


(defun get-projection (token sentence)
  (let ((head-id (token-head token))
	(arange (if (< (token-head token) (token-id token))
                    (range (1+ (token-head token)) (token-id token))
                    (range (token-id token) (1- (token-head token))))))
    (remove-if-not (lambda (tk)
                     (is-descendant? tk head-id sentence))
		   arange)))


(defun is-token-projective (token sentence)
  (if (equal 0 (token-head token))
      (values t nil)
      (let* ((prj (get-projection token sentence))
	     (arange (if (< (token-head token) (token-id token))
			 (range (1+ (token-head token)) (token-id token))
			 (range (token-id token) (1- (token-head token)))))
	     (aset   (sort (set-difference arange prj :test #'equal) #'<= )))
	(values (not aset) aset))))


(defun is-sentence-projective (sentence)
  (let ((tks (remove-if (lambda (tk) (is-token-projective tk sentence))
			(sentence-tokens sentence))))
    (values (not tks) tks)))


(defun validate-punct (sentence)
  (let ((errors))
    (dolist (tk (sentence-tokens sentence) errors)
      (multiple-value-bind (is-proj ids)
	  (is-token-projective tk sentence)
	(when (not is-proj)
	  (if (equal "PUNCT" (token-upostag tk))
	      (push (list tk 'punct-is-nonproj-over ids) errors)
	      (dolist (id ids)
		(let ((ct (sentence-get-token-by-id sentence id)))
		  (if (and (equal "PUNCT" (token-upostag ct))
			   (notany (lambda (n) (is-descendant? id n sentence)) ids))
		      (push (list ct 'punct-causes-nonproj-of (token-id tk)) errors))))))))))

