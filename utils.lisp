
(in-package :cl-conllu)

;; alternatives
;; http://stackoverflow.com/questions/3672880/more-generic-lisp-code-to-generate-combinations-of-pairs
;; http://stackoverflow.com/questions/28504467/combining-list-of-list
;; https://groups.google.com/forum/#!topic/comp.lang.lisp/NrMe2NmcCLU
;; https://goo.gl/KjclLT

(defun levenshtein (s1 s2 &key test)
  (let* ((width (1+ (length s1)))
	 (height (1+ (length s2)))
	 (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
	(setf (aref d (1+ y) (1+ x))
	      (min (1+ (aref d y (1+ x)))
		   (1+ (aref d (1+ y) x))
		   (+ (aref d y x)
		      (if (funcall test (aref s1 x) (aref s2 y))
			  0
			  1))))))
    (aref d (1- height) (1- width))))


(defun diff (sentences-a sentences-b &key test key)
  (let* ((dim (list (length sentences-a)
		    (length sentences-b)))
	 (distances (make-array dim :initial-element -1)))
    (dotimes (line (car dim) distances)
      (format t "Computing line [~a]~%" line)
      (dotimes (column (cadr dim))
	(setf (aref distances line column)
	      (funcall test
		       (funcall key (nth line sentences-a))
		       (funcall key (nth column sentences-b))))))))


(defun print-diff (matriz)
  (mapcar (lambda (line)
	    (cons (sentence-meta (car line))
		  (mapcar (lambda (pair)
			    (list (sentence-meta (car pair))
				  (cadr pair)))
			  (cdr line))))
	  matriz))


(defun find-min (list &key (min nil))
  (cond
    ((null list)
     min)
    ((or (null min)
	 (< (caddr (car list))
	    (caddr min)))
     (find-min (cdr list) :min (car list)))
    (t (find-min (cdr list) :min min))))


(defun insert-at (lst index newelt)
  (if (equal index 0)
      (cons newelt lst)
      (progn
	(push newelt (cdr (nthcdr (1- index) lst)))
	lst)))


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
 		   ;; (format t "Filtered: ~a~%" (pop interval)) ;; if uncommenting this, comment line below
		   (pop interval)
		   (filter-interval pair))))
	(dolist (pair pairs)
 	  ;; (format t "Pair: ~% ~a ~% Stack: ~% ~a ~%--~%" pair interval)
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

