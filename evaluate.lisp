
(in-package :cl-conllu)


;; Functions for evaluating parsers: comparing the conllu output of a parser with a golden parse file
;; Comparisons below are for dependency only (arcs and labels)


;; two sentence objects
;; check if same words (by id and form) (it should be same POS, and same Features, but this won't affect)
;; run evaluation

(defvar *deprel-value-list*
  '("nsubj"
    "obj"
    "iobj"
    "csubj"
    "ccomp"
    "xcomp"
    "obl"
    "vocative"
    "expl"
    "dislocated"
    "advcl"
    "advmod"
    "discourse"
    "aux"
    "cop"
    "mark"
    "nmod"
    "appos"
    "nummod"
    "acl"
    "amod"
    "det"
    "clf"
    "case"
    "conj"
    "cc"
    "fixed"
    "flat"
    "compound"
    "list"
    "parataxis"
    "orphan"
    "goeswith"
    "reparandum"
    "punct"
    "root"
    "dep")
  "List of the 37 universal syntactic relations in UD.")

(defun mappend (function list)
  "Receives a function and a list of lists and returns the appended
  result of the aplication of the function to each list."
  (apply #'append (mapcar function list)))

(defun disagreeing-words (sent1 sent2 &key (head-error t) (label-error t) (remove-punct nil))
  "Returns a list of disagreements in dependency parsing (either head
   or label):

   a list (w1,w2) where w1 is a word of sent1, w2 is its matching word
   on sent2 and they disagree.

   If head-error is true, getting the wrong head is considered a
   disagreement (an error).
   If label-error is true, getting the wrong label (type of dependency
   to head) is considered a disagreement (an error).
   By default both are errors.

   We assume that sent1 is the classified result and sent2 is the
   golden (correct) sentence."
  (assert
   (every #'identity
	  (mapcar
	   #'(lambda (tk1 tk2)
	       (and (equal (token-id tk1)
			(token-id tk2))
		    (equal (token-form tk1)
			(token-form tk2))))
	   (sentence-tokens sent1)
	   (sentence-tokens sent2)))
   ()
   "Error: Sentence words do not match. The sentence pair ID is: ~a, ~a~%"
   (sentence-id sent1)
   (sentence-id sent2))
  (assert
   (or head-error
       label-error)
   ()
   "Error: At least one error must be used!")
  (remove-if
   (lambda (x)
     (or
      (and remove-punct
	   (equal (token-upostag (first x))
		  "PUNCT"))
      (and      
       (or (not head-error)
	   (equal (token-head (first x))
		  (token-head (second x))))
       (or (not label-error)
	   (equal (token-deprel (first x))
		  (token-deprel (second x)))))))
   (mapcar
    #'list
    (sentence-tokens sent1)
    (sentence-tokens sent2))))

(defun attachment-score (list-sent1 list-sent2 &key (labeled nil) (remove-punct nil))
  "Attachment score by word (micro-average).

   The attachment score is the percentage of words that have correct
   arcs to their heads.

   The unlabeled attachment score (UAS) considers only who is the head
   of the token, while the labeled attachment score (LAS) considers
   both the head and the arc label (dependency label / syntactic class).

   References:
     - Dependency Parsing - Kubler, Mcdonald and Nivre (pp.79-80)"
  (let ((total-words
	 (reduce #'+
		 list-sent1
		 :key #'sentence-size
		 :initial-value 0))
	(wrong-words
	 (reduce #'+
		 (mapcar #'(lambda (x y)
			   (disagreeing-words x y :label-error labeled :remove-punct remove-punct))
		       list-sent1
		       list-sent2)
		 :key #'length
		 :initial-value 0)))
    (/ (- total-words
	  (float wrong-words))
       total-words)))

(defun attachment-score-sentence (list-sent1 list-sent2 &key labeled)
  "Attachment score by sentence (macro-average).

   The attachment score is the percentage of words that have correct
   arcs to their heads.

   The unlabeled attachment score (UAS) considers only who is the head
   of the token, while the labeled attachment score (LAS) considers
   both the head and the arc label (dependency label / syntactic class).

   References:
     - Dependency Parsing - Kubler, Mcdonald and Nivre (pp.79-80)"
  (let ((total-sentences
	 (length list-sent1))
	(wrong-sentences
	 (reduce
	  #'+
	  (mapcar
	   #'(lambda (x y)
	       (disagreeing-words x y :label-error labeled))
	   list-sent1
	   list-sent2)
	  :key #'(lambda (wrong-words)
		   (if wrong-words
		       1 			; There's at least one wrong word
		       0))			; There's no wrong word
	  :initial-value 0)))
    (/ (- total-sentences
	  (float wrong-sentences))
       total-sentences)))

(defun recall (list-sent1 list-sent2 deprel &key (head-error nil) (label-error t))
  "Restricted to words which are originally of syntactical class
  (dependency type to head) `deprel`, returns the recall:
   the number of true positives divided by the number of words
   originally positive (that is, correctly of class `deprel`).

   head-error and label-error define what is considered an error (a
   false negative)"
  (assert
   (or head-error
       label-error)
   ()
   "Error: At least one error must be used!")
  (let ((total-words
	 (length
	  (remove-if-not
	   #'(lambda (x)
	       (equal x deprel))
	   (mappend #'sentence-tokens
		    list-sent2)
	   :key #'token-deprel)))
	(wrong-words
	 (length
	  (remove-if-not
	   #'(lambda (x)
	       (equal x deprel))
	   (mappend
	    #'identity
	    (mapcar
	     #'(lambda (sent1 sent2)
		 (disagreeing-words
		  sent1 sent2
		  :head-error head-error
		  :label-error label-error))
	     list-sent1
	     list-sent2))
	   :key #'(lambda (disag-pair)
		    (token-deprel
		     (second disag-pair)))))))
    (if (eq total-words
	    0)
	nil
	(/ (float (- total-words wrong-words))
	   total-words))))

(defun precision (list-sent1 list-sent2 deprel &key (head-error nil) (label-error t))
  "Restricted to words which are originally of syntactical class
   (dependency type to head) `deprel`, returns the precision:
   the number of true positives divided by the number of words
   predicted positive (that is, predicted as of class `deprel`).

   head-error and label-error define what is considered an error (a
   false positive)"
  (assert
   (or head-error
       label-error)
   ()
   "Error: At least one error must be used!")
  (let ((classified-words
	 (length
	  (remove-if-not
	   #'(lambda (x)
	       (equal x deprel))
	   (mappend #'sentence-tokens
		    list-sent1)
	   :key #'token-deprel)))
	(wrong-words
	 (length
	  (remove-if-not
	   #'(lambda (x)
	       (equal x deprel))
	   (mappend
	    #'identity
	    (mapcar
	     #'(lambda (sent1 sent2)
		 (disagreeing-words
		  sent1 sent2
		  :head-error head-error
		  :label-error label-error))
	     list-sent1
	     list-sent2))
	   :key #'(lambda (disag-pair)
		    (token-deprel
		     (first disag-pair)))))))
    (if (eq classified-words
	    0)
	nil
	(/ (float (- classified-words wrong-words))
	   classified-words))))

;; compare to baseline (random considering incidence of each class)
(defun confusion-matrix (list-sent1 list-sent2)
  "Returns a hash table where keys are lists (deprel1 deprel2) and
values are fraction of classifications as deprel1 of a word that
originally was deprel2."
  (let* ((M (make-hash-table :test #'equal))
	 (all-words-pair-list
	  (mapcar
	   #'list
	   (mappend #'sentence-tokens list-sent1)
	   (mappend #'sentence-tokens list-sent2)))
	 (N (coerce (length all-words-pair-list) 'float)))
    (assert
     (every #'identity
	    (mapcar
	     #'(lambda (pair)
		 (let ((tk1 (first pair))
		       (tk2 (second pair)))
		   (and (equal (token-id tk1)
			       (token-id tk2))
			(equal (token-form tk1)
			       (token-form tk2)))))
	     all-words-pair-list))
     ()
     "Error: Sentence words do not match.")
    
    (dolist (rel1 *deprel-value-list*)
      (dolist (rel2 *deprel-value-list*)
	(setf (gethash `(,rel1 ,rel2) M) 0)))
    
    (dolist (pair all-words-pair-list)
      (incf (gethash
	     (mapcar #'token-deprel
		     pair)
	     M)))
    
    (dolist (rel1 *deprel-value-list* M)
      (dolist (rel2 *deprel-value-list*)
	(setf (gethash `(,rel1 ,rel2) M)
	      (/ (gethash `(,rel1 ,rel2) M)
		 N))))))

(defun format-matrix (matrix)
  (format t "~{~15a |~^ ~}~%" (cons " " *deprel-value-list*))
  (dolist (dep1 *deprel-value-list*)
    (let ((L (reverse (remove-if-not #'(lambda (x) (equal x dep1)) matrix :key #'(lambda (x) (first (car x)))))))
      (format t "~{~15a |~^ ~}~%"
	      (cons dep1 (mapcar #'(lambda (x) (cdr x)) L))))))
