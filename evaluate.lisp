
(in-package :cl-conllu)

;; Functions for evaluating parsers


(defvar *deprel-value-list*
  '("nsubj"
    "obj"   "iobj"
    "csubj" "ccomp" "xcomp"
    "obl"   "vocative"
    "expl"  "dislocated"
    "advcl" "advmod"
    "discourse"
    "aux"  "cop"
    "mark" "nmod" "appos"
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


(defun simple-deprel (deprel)
  (car (ppcre:split ":" deprel)))


(defun token-equal (tk1 tk2 &key (fields *token-fields*) (test #'equal) (simple-dep nil))
  (every (lambda (field)
	   (if (and (equal field 'deprel) simple-dep)
	       (funcall test
			(simple-deprel (slot-value tk1 field))
			(simple-deprel (slot-value tk2 field)))
	       (funcall test
			(slot-value tk1 field)
			(slot-value tk2 field))))
	 fields))


(defun token-diff (tk1 tk2 &key (fields *token-fields*) (test #'equal) (simple-dep nil))
  (loop for field in fields
	for res = (if (and (equal field 'deprel) simple-dep)
		      (funcall test
			       (simple-deprel (slot-value tk1 field))
			       (simple-deprel (slot-value tk2 field)))
		      (funcall test
			       (slot-value tk1 field)
			       (slot-value tk2 field)))
	unless res
	collect (list field (slot-value tk1 field) (slot-value tk2 field))))


(defun sentence-diff (sent1 sent2 &key (fields *token-fields*)
				    (test #'equal) (simple-dep nil) (punct t))
  (assert (equal (sentence-size sent1) (sentence-size sent2)))
  (loop for tk1 in (remove-if (lambda (tk)
				(and (not punct) (equal "PUNCT" (token-upostag tk))))
			      (sentence-tokens sent1))
	for tk2 in (remove-if (lambda (tk)
				(and (not punct) (equal "PUNCT" (token-upostag tk))))
			      (sentence-tokens sent2))
	for diff = (token-diff tk1 tk2 :fields fields :test test :simple-dep simple-dep) 
	when diff
	collect (list (token-id tk1) diff)))


(defun attachment-score-by-sentence (list-sent1 list-sent2 &key (fields *token-fields*)
							     (punct t) (simple-dep nil))
  "Attachment score by sentence (macro-average).

   The attachment score is the percentage of words that have correct
   arcs to their heads. The unlabeled attachment score (UAS) considers
   only who is the head of the token, while the labeled attachment
   score (LAS) considers both the head and the arc label (dependency
   label / syntactic class).

   References:
     - Dependency Parsing - Kubler, Mcdonald and Nivre (pp.79-80)"
  (let ((ns (mapcar #'(lambda (x y)
			(- 1.0
			   (/ (length (sentence-diff x y :fields fields
						     :punct punct
						     :simple-dep simple-dep))
			      (sentence-size y))))
		    list-sent1 list-sent2)))
    (/ (apply #'+ ns) (float (length ns)))))


(defun attachment-score-by-word (list-sent1 list-sent2 &key (fields *token-fields*)
							 (punct t) (simple-dep nil))
  "Attachment score by word (micro-average). See also the
  `attachment-score-by-sentence`.

   References:
     - Dependency Parsing - Kubler, Mcdonald and Nivre (pp.79-80)"
  (let ((total-words (apply #'+ (mapcar #'sentence-size list-sent1)))
	(wrong-words (reduce #'+
			     (mapcar #'(lambda (x y)
					 (sentence-diff x y :fields fields
							:punct punct
							:simple-dep simple-dep))
				     list-sent1
				     list-sent2)
			     :key #'length
			     :initial-value 0)))
    (- 1.0 (/ wrong-words total-words))))


(defun recall (list-sent1 list-sent2 deprel &key (head-error nil) (label-error t) (simple-deprel nil))
  "Restricted to words which are originally of syntactical class
  (dependency type to head) `deprel`, returns the recall:
   the number of true positives divided by the number of words
   originally positive (that is, correctly of class `deprel`).

   head-error and label-error define what is considered an error (a
   false negative)"
  (labels ((token-deprel-chosen (tk)
	     (if simple-deprel
		 (token-simple-deprel tk)
		 (token-deprel tk))))
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
	     :key #'token-deprel-chosen)))
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
		      (token-deprel-chosen
		       (second disag-pair)))))))
      (if (eq total-words
	      0)
	  nil
	  (/ (float (- total-words wrong-words))
	     total-words)))))

(defun precision (list-sent1 list-sent2 deprel &key (head-error nil)
						 (label-error t) (simple-deprel nil))
  "Restricted to words which are classified as of syntactical class
   (dependency type to head) `deprel`, returns the precision:
   the number of true positives divided by the number of words
   predicted positive (that is, predicted as of class `deprel`).

   head-error and label-error define what is considered an error (a
   false positive)"
  (labels ((token-deprel-chosen (tk)
	     (if simple-deprel
		 (token-simple-deprel tk)
		 (token-deprel tk))))
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
	     :key #'token-deprel-chosen)))
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
		      (token-deprel-chosen
		       (first disag-pair)))))))
      (if (eq classified-words
	      0)
	  nil
	  (/ (float (- classified-words wrong-words))
	     classified-words)))))

(defun projectivity-accuracy (list-sent1 list-sent2)
  (let ((N (length list-sent1))
	(correct 0))
    (mapcar
     #'(lambda (x y)
       (if (eq (non-projective? x)
	       (non-projective? y))
	   (incf correct)))
     list-sent1
     list-sent2)
    (if (eq N 0)
	nil
	(/ (float correct)
	   N))))

(defun projectivity-precision (list-sent1 list-sent2)
  (let ((number-of-positives
	 (length (remove-if-not
		  #'non-projective?
		  list-sent1)))
	(true-positives 0))
    (mapcar
     #'(lambda (x y)
	 (if (and
	      (eq (non-projective? x) t)
	      (eq (non-projective? y) t))
	     (incf true-positives)))
     list-sent1
     list-sent2)
    (if (eq 0
	    number-of-positives)
	nil
	(/ (float true-positives)
	   number-of-positives))))

(defun projectivity-recall (list-sent1 list-sent2)
  (let ((number-of-projectives
	 (length (remove-if-not
		  #'non-projective?
		  list-sent2)))
	(true-positives 0))
    (mapcar
     #'(lambda (x y)
	 (if (and
	      (eq (non-projective? x) t)
	      (eq (non-projective? y) t))
	     (incf true-positives)))
     list-sent1
     list-sent2)
    (if (eq 0
	    number-of-projectives)
	nil
	(/ (float true-positives)
	   number-of-projectives))))


(defun confusion-matrix (list-sent1 list-sent2 &key (normalize t))
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
	     (mapcar #'token-simple-deprel
		     pair)
	     M)))

    (if normalize
	(dolist (rel1 *deprel-value-list* M)
	  (dolist (rel2 *deprel-value-list*)
	    (if (not
		 (eq 0
		     (gethash `(,rel1 ,rel2) M)))
		(setf (gethash `(,rel1 ,rel2) M)
		      (/ (gethash `(,rel1 ,rel2) M)
			 N))))))
    M))


(defun format-matrix (matrix)
  (let ((M (alexandria:hash-table-alist matrix)))
    (format t "~{~15a |~^ ~}~%" (cons " " *deprel-value-list*))
    (dolist (dep1 *deprel-value-list*)
      (let ((L (reverse (remove-if-not #'(lambda (x) (equal x dep1)) M
				       :key #'(lambda (x) (first (car x)))))))
	(format t "~{~15a |~^ ~}~%"
		(cons dep1 (mapcar #'(lambda (x) (cdr x)) L)))))))

