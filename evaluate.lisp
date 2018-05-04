
(in-package :conllu.evaluate)

;; Functions for evaluating parsers

(defparameter *token-fields*
  '(id form lemma upostag xpostag feats head deprel deps misc))

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
                                    (test #'equal) (simple-dep nil) (ignore-punct nil))
  "Returns a list of differences in SENT1 and SENT2.

   They must have the same size.

   If IGNORE-PUNCT, tokens which have 'PUNCT' as upostag
   in sent2 are ignored."
  
  (assert (equal (sentence-size sent1) (sentence-size sent2)))
  (let ((complete-diff
	 (loop for tk1 in (sentence-tokens sent1)
	    for tk2 in (sentence-tokens sent2)
	    for diff = (token-diff tk1 tk2 :fields fields :test test :simple-dep simple-dep)
	    when diff
	    collect (list (token-id tk1) diff))))
    (if ignore-punct
	(remove-if
	 #'(lambda (diff-entry)
	     (let ((diff-entry-id (first diff-entry)))
	       (equal "PUNCT"
		      (token-upostag (find diff-entry-id
					   (sentence-tokens sent2)
					   :key #'token-id)))))
	 complete-diff)
	complete-diff)))


(defun sentence-average-score (list-sent1 list-sent2
                               &key (fields *token-fields*)
                                 (ignore-punct t) (simple-dep nil))
  "Score by sentence (macro-average).

   This is the mean of the percentage of words in each sentence
   that are correct with respect to the fields in FIELDS.

   We assume that LIST-SENT1 is the classified result
   and LIST-SENT2 is the list of golden (correct) sentences.

   If IGNORE-PUNCT, tokens which have 'PUNCT' as upostag
   in the golden sentences are ignored."
  
  (let ((list-of-scores
         (mapcar
          #'(lambda (x y)
              (- 1.0
                 (/ (length (sentence-diff
                             x y
                             :fields fields
                             :ignore-punct ignore-punct
                             :simple-dep simple-dep))
                    (sentence-size y))))
          list-sent1 list-sent2)))
    (/ (apply #'+ list-of-scores)
       (float (length list-of-scores)))))


(defun word-average-score (list-sent1 list-sent2
                           &key (fields *token-fields*)
                             (ignore-punct t) (simple-dep nil))
  "Score by word (micro-average).

   This is the total mean of words in all sentences that
   are correct with respect to the fields in FIELDS.

   We assume that LIST-SENT1 is the classified result
   and LIST-SENT2 is the list of golden (correct) sentences.

   If IGNORE-PUNCT, tokens which have 'PUNCT' as upostag
   in the golden sentences are ignored."

  (let ((total-words (apply #'+ (mapcar #'sentence-size list-sent1)))
        (wrong-words (reduce #'+
                             (mapcar
                              #'(lambda (x y)
                                  (sentence-diff
                                   x y
                                   :fields fields
                                   :ignore-punct ignore-punct
                                   :simple-dep simple-dep))
                              list-sent1
                              list-sent2)
                             :key #'length
                             :initial-value 0)))
    (- 1.0 (/ wrong-words total-words))))


(defun attachment-score-by-sentence (list-sent1 list-sent2
                                     &key (labeled t)
                                       (ignore-punct nil) (simple-dep nil))
  "Attachment score by sentence (macro-average).

   The attachment score is the percentage of words that have correct
   arcs to their heads. The unlabeled attachment score (UAS) considers
   only who is the head of the token, while the labeled attachment
   score (LAS) considers both the head and the arc label (dependency
   label / syntactic class).

   In order to choose between labeled or unlabeled,
   set the key argument LABELED.

   References:
     - Dependency Parsing. Kubler, Mcdonald and Nivre (pp.79-80)"
  
  (sentence-average-score list-sent1 list-sent2
                          :fields (if labeled
                                      '(head deprel)
                                      '(head))
                          :ignore-punct ignore-punct
                          :simple-dep simple-dep))


(defun attachment-score-by-word (list-sent1 list-sent2
				 &key (labeled t)
				   (ignore-punct nil) (simple-dep nil))

  "Attachment score by word (micro-average).

   The attachment score is the percentage of words that have correct
   arcs to their heads. The unlabeled attachment score (UAS) considers
   only who is the head of the token, while the labeled attachment
   score (LAS) considers both the head and the arc label (dependency
   label / syntactic class).

   In order to choose between labeled or unlabeled,
   set the key argument LABELED.

   References:
     - Dependency Parsing. Kubler, Mcdonald and Nivre (pp.79-80)"

  (word-average-score list-sent1 list-sent2
		      :fields (if labeled
				  '(head deprel)
				  '(head))
		      :ignore-punct ignore-punct
		      :simple-dep simple-dep))


(defun recall (list-sent1 list-sent2 deprel
               &key (error-type '(deprel)) (simple-dep nil))
  "Restricted to words which are originally of syntactic class
  (dependency type to head) DEPREL, returns the recall:
   the number of true positives divided by the number of words
   originally positive (that is, originally of class DEPREL).

   We assume that LIST-SENT1 is the classified result
   and LIST-SENT2 is the list of golden (correct) sentences.

   ERROR-TYPE defines what is considered an error (a false negative).
   Some usual values are:
    - '(deprel) :: for the deprel tagging task only
    - '(head) :: for considering errors for each syntactic class
    - '(deprel head) :: for considering correct only when both deprel
                        and head are correct."

  (labels ((token-deprel-chosen (tk)
             (if simple-dep
                 (simple-deprel (token-deprel tk))
                 (token-deprel tk))))
    (assert
     (and (listp error-type)
          (not (null error-type)))
     ()
     "Error: ERROR-TYPE should be a list and at least one error must be used!")
    (let ((total-words
           (length
            (remove-if-not
             #'(lambda (x)
                 (equal x deprel))
             (alexandria:mappend #'sentence-tokens
                      list-sent2)
             :key #'token-deprel-chosen)))
          (wrong-words
           (length
            (mapcar
             #'(lambda (sent1 sent2)
                 (remove-if-not
                  #'(lambda (x)
                      (equal x deprel))
                  (sentence-diff
		   sent1 sent2
		   :fields error-type
		   :simple-dep simple-dep)
                  :key
                  ;; deprel in the original (SENT2) sentence
                  #'(lambda (diff-entry)
                      (let ((diff-entry-id (first diff-entry)))
                        (token-deprel-chosen
                         (find diff-entry-id
                               (sentence-tokens sent2)
                               :key #'token-id))))))
             list-sent1
             list-sent2))))
      
      (if (eq total-words
	      0)
	  (warn "There are no tokens correctly of deprel ~a." deprel)
	  (/ (float (- total-words wrong-words))
	     total-words)))))

(defun precision (list-sent1 list-sent2 deprel
		  &key (error-type '(deprel)) (simple-dep nil))
  "Restricted to words which are classified as of syntactical class
   (dependency type to head) DEPREL, returns the precision:
   the number of true positives divided by the number of words
   predicted positive (that is, predicted as of class DEPREL).

   We assume that LIST-SENT1 is the classified (predicted) result
   and LIST-SENT2 is the list of golden (correct) sentences.

   ERROR-TYPE defines what is considered an error (a false negative).
   Some usual values are:
    - '(deprel) :: for the deprel tagging task only
    - '(head) :: for considering errors for each syntactic class
    - '(deprel head) :: for considering correct only when both deprel
                        and head are correct."
  
  (labels ((token-deprel-chosen (tk)
	     (if simple-dep
		 (simple-deprel (token-deprel tk))
		 (token-deprel tk))))
    (assert
     (and (listp error-type)
	  (not (null error-type)))
     ()
     "Error: ERROR-TYPE should be a list and at least one error must be used!")
    
    (let ((classified-words
	   (length
	    (remove-if-not
	     #'(lambda (x)
		 (equal x deprel))
	     (alexandria:mappend #'sentence-tokens
		      list-sent1)
	     :key #'token-deprel-chosen)))
	  (wrong-words
	   (length
	    (mapcar
	     #'(lambda (sent1 sent2)
		 (remove-if-not
		  #'(lambda (x)
		      (equal x deprel))
		  (sentence-diff
		   sent1 sent2
		   :fields error-type
		   :simple-dep simple-dep)
		  :key
		  ;; deprel in the predicted (SENT1) sentence
		  #'(lambda (diff-entry)
		      (let ((diff-entry-id (first diff-entry)))
			(token-deprel-chosen
			 (find diff-entry-id
			       (sentence-tokens sent1)
			       :key #'token-id))))))
	     list-sent1
	     list-sent2))))
      (if (eq classified-words
	      0)
	  (warn "There are no tokens predicted as of deprel ~a." deprel)
	  (/ (float (- classified-words wrong-words))
	     classified-words)))))

(defun non-projectivity-accuracy (list-sent1 list-sent2)
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
	(error "LIST-SENT1 is empty!")
	(/ (float correct)
	   N))))

(defun non-projectivity-precision (list-sent1 list-sent2)
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
	(warn "There are no non-projective sentences in LIST-SENT1")
	(/ (float true-positives)
	   number-of-positives))))

(defun non-projectivity-recall (list-sent1 list-sent2)
  (let ((number-of-non-projectives
	 (length (remove-if-not
		  #'non-projective?
		  list-sent2)))
	(true-positives 0))
    (mapcar
     #'(lambda (x y)
	 (if (and
	      (non-projective? x)
	      (non-projective? y))
	     (incf true-positives)))
     list-sent1
     list-sent2)
    (if (eq 0
	    number-of-non-projectives)
	(warn "There are no non-projective sentences in LIST-SENT2")
	(/ (float true-positives)
	   number-of-non-projectives))))


(defun exact-match-sentence (sent1 sent2 &key (compared-fields '(upostag feats head deprel)) (identity-fields '(id form)) (test #'equal) (simple-dep nil) (ignore-punct nil))
  "Compares if two sentences are an exact match.

   The typical use case is comparing the result of a tagger (or
   parser) against a test sentence.

   Returns SENT1 if SENT1 and SENT2 agree for all tokens with respect to
   the COMPARED-FIELDS. Otherwise returns NIL.

   If they do not have the same number of tokens or if the tokens do
   not agree on each IDENTITY-FIELD, then the sentences are not 'the
   same' and thus an error is returned."

  (assert (null (sentence-diff sent1 sent2 :fields identity-fields)))

  (if (sentence-diff sent1 sent2 :fields compared-fields
		     :test test
		     :simple-dep simple-dep
		     :ignore-punct ignore-punct)
      nil
      sent1))

(defun exact-match (list-sent1 list-sent2 &key (compared-fields '(upostag feats head deprel)) (identity-fields '(id form)) (test #'equal) (simple-dep nil) (ignore-punct nil))
  "Returns the list of sentences of LIST-SENT1 that are an exact
  match to the corresponding sentence of LIST-SENT2 (same position in
  list).

  LIST-SENT1 and LIST-SENT2 must have the same size with corresponding
  sentences in order."
  (assert (equal
	   (length list-sent1)
	   (length list-sent2)))
  (remove nil
	  (mapcar #'(lambda (x y)
		      (exact-match-sentence
		       x y
		       :compared-fields compared-fields
		       :identity-fields identity-fields
		       :test test
		       :simple-dep simple-dep
		       :ignore-punct ignore-punct))
		  list-sent1
		  list-sent2)
	  :test #'equal))

(defun exact-match-score (list-sent1 list-sent2 &key (compared-fields '(upostag feats head deprel)) (identity-fields '(id form)) (test #'equal) (simple-dep nil) (ignore-punct nil))
  "Returns the percentage of sentences of LIST-SENT1 that are an exact
  match to LIST-SENT2.

  LIST-SENT1 and LIST-SENT2 must have the same size with corresponding
  sentences in order.

  The typical use case is comparing the result of a tagger (or
  parser) against a test set, where an exact match is a completely
  correct tagging (or parse) for the sentence.

  References:
    - Dependency Parsing. Kubler, Mcdonald and Nivre (p.79)"

  (assert (equal
	   (length list-sent1)
	   (length list-sent2)))
  (let ((n (float (length list-sent1)))
	(correct-sentences
	 (length
	  (exact-match
	   list-sent1 list-sent2
	   :compared-fields compared-fields
	   :identity-fields identity-fields
	   :test test
	   :simple-dep simple-dep
	   :ignore-punct ignore-punct))))
    (/ correct-sentences n)))

