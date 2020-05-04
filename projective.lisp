
(in-package :cl-conllu)

(defun range (n m)
  (loop for x from n upto (- m 1) collect x))

(defun get-token-by-id (id sentence)
  (find id (sentence-tokens sentence) :key #'token-id))


(defun get-projection (token sentence &key (projection nil))
  (dolist (child (mapcar #'token-id (token-children token sentence)) projection)
    (if (not (member child projection :test #'equal))
	(get-projection (find child (sentence-tokens sentence) :key #'token-id)
			sentence
			:projection (adjoin child projection :test #'equal)))))


(defun get-gap (token sentence)
  (let* ((token-id     (token-id token))
	 (head-id      (token-head token))
	 (head         (get-token-by-id head-id sentence))
	 (rangebetween (if (< token-id head-id)
			   (range (+ 1 token-id) (- head-id 1))
			   (range (+ 1 head-id)  (- token-id 1)))))
    (if rangebetween
	(set-difference rangebetween (get-projection head sentence)))))


(defun get-token-ancestors (token sentence &key (ancestors nil))
  (let ((head-id (token-head token)))
    (cond ((equal 0 head-id)
	   (append ancestors (list 0)))
	  ((member head-id ancestors)
	   (error "cycle detected ~a" sentence))
	  (t (get-token-ancestors (get-token-by-id head-id sentence)
				  sentence
				  :ancestors (append ancestors (list head-id)))))))


(defun get-caused-nonprojectivities (token sentence)
  (let ((ancestors (get-token-ancestors token sentence))
	(max-id    (apply #'max (mapcar #'token-id (sentence-tokens sentence))))
	(head-id   (token-head token))
	(token-id  (token-id token)))
    (let* ((left  (if (< head-id token-id)
		      (range (+ head-id 1) token-id)
		      (range 1 token-id)))
	   (right (if (< head-id token-id)
		      (range (+ token-id 1) (+ max-id 1))
		      (range (+ token-id 1) head-id)))
	   (leftcross (remove-if-not (lambda (n)
				       (let ((tk-n (find n (sentence-tokens sentence) :key #'token-id)))
					 (> (token-head tk-n) token-id)))
				     (set-difference left ancestors :test #'equal)))
	   (rightcross (remove-if-not (lambda (n)
					(let ((tk-n (find n (sentence-tokens sentence) :key #'token-id)))
					  (< (token-head tk-n) token-id)))
				      (set-difference right ancestors :test #'equal))))
      (if (< head-id token-id)
	  (setf rightcross
		(remove-if-not (lambda (n)
				 (let ((tk-n (find n (sentence-tokens sentence) :key #'token-id)))
				   (> (token-head tk-n) head-id)))
			       rightcross))
	  (setf leftcross
		(remove-if-not (lambda (n)
				 (let ((tk-n (find n (sentence-tokens sentence) :key #'token-id)))
				   (< (token-head tk-n) head-id)))
			       leftcross)))
      (sort (append leftcross rightcross) #'<=))))


(defun validate-projective-punctuation (token sentence)
  (if (equal "punct" (token-deprel token))
      (let ((nonprojnodes (get-caused-nonprojectivities token sentence))
	    (gap          (get-gap token sentence)))
	(if nonprojnodes
	    (error "Punctuation must not cause non-projectivity of nodes ~a" nonprojnodes))
	(if gap
	    (error "Punctuation must not be attached non-projectively over nodes ~a" gap)))))


(defun non-projective? (sentence)
  "Verifies if a sentence tree is projective. Intuitively, this means
  that, keeping word order, there's no two dependency arcs that cross.
 
   More formally, let i -> j mean that j's head is node i. Let '->*'
   be the transitive closure of '->'.

   A tree if projective when, for each node i, j: if i -> j, then for
   each node k between i and j (i < k < j or j < k < i), i ->* k.

   References:
    - Nivre, Joakim; Inductive Dependency Parsing, 2006
    - https://en.wikipedia.org/wiki/Discontinuity_(linguistics)"
  ;; Puts every undirected pair into a list
  ;; Sorts list of pairs
  ;; check crosses with a stack of pairs and passing through the list
  ;; --
  ;; formats are only for debugging
  ;; computational complexity is bounded by sorting (Omega(n log(n)))
  (let ((pairs nil))
    (mapc
     #'(lambda (tk) (push (if (< (token-id tk)
				 (token-head tk))
			      (list (token-id tk)
				    (token-head tk))
			      (list (token-head tk)
				    (token-id tk)))
			  pairs))
     (sentence-tokens sentence))
     ;; (format t "~a~%" pairs)
    (setf pairs
	  (sort pairs
		#'(lambda (pair-1 pair-2)
		    (cond
		      ((< (first pair-1)
			  (first pair-2))
		       t)
		      ((= (first pair-1)
			  (first pair-2))
		       (if (> (second pair-1)
			      (second pair-2))
			   t
			   nil))
		      ((> (first pair-1)
			  (first pair-2))
		       nil)))))
     ;; (format t "~a~%" pairs)
    (let ((interval
	   ;; Stack of intervals
	   (list
	    (list 0 (sentence-size sentence)))))
      (labels ((filter-interval (pair)
		 (when (>= (first pair)
			   (second (car interval)))
		   (pop interval)
		   (filter-interval pair))))
	(dolist (pair pairs)
	  (filter-interval pair)
	  (if (and
	       (> (first pair)
		  (first (car interval)))
	       (> (second pair)
		  (second (car interval))))
	      ;; Not projective!
	      (return (list (car interval) pair))
	      ;; No violation
	      (push pair interval)))))))
