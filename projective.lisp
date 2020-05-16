
(in-package :cl-conllu)

#|

Verifies if a sentence tree is projective. Intuitively, this means
that, keeping word order, there's no two dependency arcs that cross.
 
More formally, let i -> j mean that j's head is node i. Let '->*' be
the transitive closure of '->'. A tree if projective when, for each
node i, j: if i -> j, then for each node k between i and j (i < k < j
or j < k < i), i ->* k.

References:

- Nivre, Joakim; Inductive Dependency Parsing, 2006
- S. Kubler, R. McDonald, and J. Nivre, Dependency Parsing. 2009,
  pp. 1–127.
- https://en.wikipedia.org/wiki/Discontinuity_(linguistics)
- https://www.aclweb.org/anthology/E06-1010.pdf

|#


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
      t
      (let* ((htk (find (token-head token) (sentence-tokens sentence) :key #'token-id :test #'equal))
	     (prj (get-projection htk sentence)))
	(if (< (token-head token) (token-id token))
	    (every (lambda (n) (member n prj))
		   (loop for x from (1+ (token-head token)) upto (1- (token-id token))
			 collect x))
	    (every (lambda (n) (member n prj))
		   (loop for x from (1+ (token-id token)) upto (1- (token-head token))
			 collect x))))))


(defun is-sentence-projective (sentence)
  (every (lambda (tk)
	   (is-token-projective tk sentence))
	 (sentence-tokens sentence)))

