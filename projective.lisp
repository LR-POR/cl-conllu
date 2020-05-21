
(in-package :cl-conllu)

#|

Verifies if a sentence tree is projective. Intuitively, this means
that, keeping word order, there's no two dependency arcs that cross.
 
More formally, let i -> j mean an arc where j's head is node i. Let
'->*' be the transitive closure of '->'. An arc is projective if for
each node k between i and j (i < k < j or j < k < i), i ->* k.

A tree is projective when all its arcs are projective. 

References:

- Nivre, Joakim; Inductive Dependency Parsing, 2006
- S. Kubler, R. McDonald, and J. Nivre, Dependency Parsing. 2009,
  pp. 1–127.
- https://en.wikipedia.org/wiki/Discontinuity_(linguistics)
- https://www.aclweb.org/anthology/E06-1010.pdf

|#


(defun range (a b)
  (assert (and (integerp a) (integerp b)))
  (cond ((= a b) (list a))
        ((< a b)
         (cons a (range (+ a 1) b)))
        ((> a b)
         (cons a (range (- a 1) b)))))


(defun get-projection (token sentence)
  (let ((pset (mapcar (lambda (tk)
			(cons (token-id tk) (token-head tk)))
		      (sentence-tokens sentence))))
    (labels ((child (tk)
	       (mapcar #'car (remove-if-not (lambda (n) (equal n tk)) pset :key #'cdr)))
	     (aux (tokens projection)
	       (if (null tokens)
		   projection
		   (aux (append (cdr tokens) (child (car tokens)))
			(adjoin (car tokens) projection)))))
      (sort (aux (list (token-id token)) nil) #'<=))))


(defun is-token-projective (token sentence)
  (if (equal 0 (token-head token))
      (values t nil)
      (let* ((htk    (find (token-head token) (sentence-tokens sentence) :key #'token-id :test #'equal))
	     (prj    (get-projection htk sentence))
	     (arange (if (< (token-head token) (token-id token))
			 (range (1+ (token-head token)) (1- (token-id token)))
			 (range (1+ (token-id token)) (1- (token-head token)))))
	     (aset   (set-difference arange prj :test #'equal)))
	(values (not aset) aset))))


(defun is-sentence-projective (sentence)
  (every (lambda (tk)
	   (is-token-projective tk sentence))
	 (sentence-tokens sentence)))



(defun validate-punct (sentence)
  (let ((error-1 nil)
	(error-2 nil)
	(tokens (sentence-tokens sentence)))
    (dolist (tk tokens (append error-1 error-2))
      (multiple-value-bind (test ids)
	  (is-token-projective tk sentence)
	(if (not test)
	    (if (equal "PUNCT" (token-upostag tk))
		(push (list 'punct-is-nonproj-over tk 
			    (mapcar (lambda (id) (nth (1- id) tokens)) ids))
		      error-2)
		(mapcar (lambda (id)
		     (let ((ct (nth (1- id) tokens)))
		       (if (equal "PUNCT" (token-upostag ct))
			   (push (list 'causes-nonproj-of ct tk) error-1))))
			ids)))))))


