(in-package :conllu.converters.tags)

(defun write-token-tag-suffix (token stream field-value separator)
  (if (null field-value)
      (format stream "~a " (token-form token))
      (format stream "~a~a~a " (token-form token)
	      separator field-value)))

(defun write-sentence-tag-suffix-to-stream (sentence &key (stream *standard-output*) (tag 'upostag) (separator "_"))
  "Writes sentence as CoNLL-U file in STREAM as FORM.SEPARATOR.TAGVALUE (without
  dots), followed by a whitespace character.

  If TAG is NIL, then writes only FORMs, followed by a whitepsace character.

  Example:
  ;; supposing sentence already defined
  (write-sentence-tag-suffix-to-stream (sentence :tag 'xpostag :sepatator \"_\"))
Pierre_NNP Vinken_NNP ,_, 61_CD years_NNS old_JJ ,_, will_MD join_VB the_DT board_NN as_IN
    a_DT nonexecutive_JJ director_NN Nov._NNP 29_CD ._.
  => NIL"
  ;; tag must be a slots
  (assert (member
	   tag
	   (cons nil
		 (mapcar #'sb-mop:slot-definition-name
			 (sb-mop:class-slots (find-class 'token))))))
  (with-slots (tokens) sentence
    (dolist (token tokens)
      (write-token-tag-suffix token stream
			      (if (null tag)
				  nil
				  (slot-value token tag))
			      separator))))

(defun write-sentences-tag-suffix-to-stream (sentences &key (stream *standard-output*) (tag 'upostag) (separator "_"))
  "See documentation for write-sentence-tag-suffix-to-stream"
  (reduce (lambda (alist sent)
	    (if alist (princ #\Newline stream))
	    (write-sentence-tag-suffix-to-stream sent
						 :stream stream
						 :tag tag
						 :separator separator)
	    (cons sent alist))
	  sentences :initial-value nil)
  (princ #\Newline stream)
  (values))

(defun write-sentences-tag-suffix (sentences filename &key (tag 'upostag) (separator "_") (if-exists :supersede))
  "See documentation for write-sentence-tag-suffix-to-stream"
  (with-open-file (out filename :direction :output :if-exists if-exists)
    (write-sentences-tag-suffix-to-stream sentences :stream out :tag tag :separator separator)))

(defun read-sentence-tag-suffix (stream field separator)
  "Writes as sentence object input from STREAM as
  FORM.SEPARATOR.TAGVALUE (without dots), followed by a whitespace
  character.

  Example:

  ;; Consider the file example.txt, with contents:
  ;; Pudim_NOUN é_VERB bom_ADJ ._PUNCT
  ;; E_CONJ torta_NOUN também_ADV ._PUNCT

  (with-open-file (s \"./example.txt\")
	     (write-conllu-to-stream (read-sentence-tag-suffix s 'upostag \"_\")))
  1	Pudim	_	NOUN	_	_	_	_	_	_
  2	é	_	VERB	_	_	_	_	_	_
  3	bom	_	ADJ	_	_	_	_	_	_
  4	.	_	PUNCT	_	_	_	_	_	_
  
  1	E	_	CONJ	_	_	_	_	_	_
  2	torta	_	NOUN	_	_	_	_	_	_
  3	também	_	ADV	_	_	_	_	_	_
  4	.	_	PUNCT	_	_	_	_	_	_
"
  (assert (and
	   (member field
		   (mapcar #'sb-mop:slot-definition-name
			   (sb-mop:class-slots (find-class 'token))))
	   (not (or (equal field 'id)
		    (equal field 'form)))))
  
  (macrolet ((my-read-line ()
	       `(read-line stream nil nil)))
    (flet ((list-to-sentence (pair-list)
	     ;; Receives a list of lists ("Form" "Field-Value")
	     ;; returns sentence with these tokens as sentence-tokens
	     (make-instance
	      'sentence
	      :tokens
	      (let ((count 0))
		(mapcar
		 #'(lambda (pair)
		     (setf count (1+ count))
		     (let ((new-token (make-instance
				       'token
				       :form (first pair)
				       :id count
				       :lemma "_")))
		       (setf (slot-value new-token field) (second pair))
		       new-token))
		 pair-list)))))
      
      (let ((sentences nil))
	(do ((line
	      (my-read-line)
	      (my-read-line)))
	    ((null line)
	     (reverse sentences))
	  (push
	   (list-to-sentence
	    (mapcar #'(lambda (x) (split separator x))
		    (cl-ppcre:split " " line)))
	   sentences))))))

(defun read-file-tag-suffix (filename &key (tag 'upostag) (separator "_"))
  (with-open-file (input filename :direction :input)
    (read-sentence-tag-suffix input tag separator)))
