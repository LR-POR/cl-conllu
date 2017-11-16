
(in-package :cl-conllu)

(defun line->token (line)
  (if (cl-ppcre:scan "^#" line)
      line
      (let ((tk (make-instance 'token)))
	(assert (equal 10 (length (cl-ppcre:split "\\t" line))))
	(mapc (lambda (value key)
		(if (member key '(id head))
		    (setf (slot-value tk key) (parse-integer value))
		    (setf (slot-value tk key) value)))
	      (cl-ppcre:split "\\t" line)
	      '(id form lemma upostag xpostag feats head deprel deps misc))
	tk)))


(defun line->mtoken (line)
  (if (cl-ppcre:scan "^#" line)
      line
      (let* ((mtk (make-instance 'mtoken))
	     (res (cl-ppcre:split "\\t" line))
	     (range (cadr (multiple-value-list (cl-ppcre:scan-to-strings "([0-9]+)-([0-9]+)" (car res))))))
	(assert (equal 10 (length res)))
	(setf (slot-value mtk 'start) (parse-integer (aref range 0))
	      (slot-value mtk 'end)   (parse-integer (aref range 1))
	      (slot-value mtk 'form)  (second res)
	      (slot-value mtk 'misc)  (car (last res)))
	mtk)))

(defun collect-meta (lines)
  (mapcar (lambda (line)
	    (let* ((cl (string-trim '(#\# #\Space #\Tab) line))
		   (pos (position #\= cl)))
              (if pos
                  (cons (subseq cl 0 (1- pos))
                        (subseq cl (+ 2 pos)))
                  (cons :raw cl))))
	  lines))


(defun make-sentence (lineno lines fn-meta)
  (labels ((reading (lines meta tokens mtokens)
	     (cond
	       ((null lines)
		(values (reverse meta) (reverse tokens) (reverse mtokens)))
	       ((cl-ppcre:scan "^#" (car lines))
		(reading (cdr lines) (cons (car lines) meta) tokens mtokens))
	       ((cl-ppcre:scan "^[0-9]+-[0-9]+\\t" (car lines))
		(reading (cdr lines) meta tokens (cons (line->mtoken (car lines)) mtokens)))
	       ((cl-ppcre:scan "^[0-9]+\\t" (car lines))
		(reading (cdr lines) meta (cons (line->token (car lines)) tokens) mtokens)))))
    (multiple-value-bind (meta tokens mtokens)
	(reading lines nil nil nil)
      (make-instance 'sentence :start lineno :tokens tokens
		     :meta (funcall fn-meta meta) :mtokens mtokens))))


(defun read-conllu (input &key (fn-meta #'collect-meta))
  (etypecase input
    (pathname
     (if (pathname-name (probe-file input))
	 (read-file input :fn-meta fn-meta)
	 (read-directory (merge-pathnames input (parse-namestring "*.conllu"))
			 :fn-meta fn-meta)))
    (string   (read-conllu (parse-namestring input) :fn-meta fn-meta))
    (stream   (read-stream input :fn-meta fn-meta))))


(defun read-directory (path &key (fn-meta #'collect-meta))
  (reduce (lambda (l n) (append l (cl-conllu:read-file n :fn-meta fn-meta)))
	  (directory path) :initial-value nil))


(defun read-file (path &key (fn-meta #'collect-meta))
  (with-open-file (in path)
    (read-stream in :fn-meta fn-meta)))


(defun read-stream (stream &key (fn-meta #'collect-meta))
  (macrolet ((flush-line ()
	       `(setq line (read-line stream nil nil)
		      lineno (+ lineno 1))))
    (prog (line (lineno 0) begining lines sentences)
     label-1
     (flush-line)
     (alexandria:switch (line :test #'equal)
       (nil (go label-3))
       ("" (go label-1))
       (t (setq begining lineno)
	  (push line lines)
	  (go label-2)))
     
     label-2
     (flush-line)
     (alexandria:switch (line :test #'equal)
       (nil (go label-3))
       ("" (push (make-sentence begining (reverse lines) fn-meta)
		 sentences)
	   (setq lines nil)
	   (go label-1))
       (t (push line lines)
	  (go label-2)))

     label-3
     (if lines
	 (push (make-sentence begining (reverse lines) fn-meta)
	       sentences))
     (return (reverse sentences)))))


;; O(2n) complexity
(defun list-to-tsv (alist stream)
  (format stream "~{~a~^~a~}"
	  (reduce (lambda (a alist)
		    (if alist
			(cons a (cons #\Tab alist))
			(cons a alist)))
		  alist :from-end t :initial-value nil)))


(defun write-token (tk stream)
  (reduce (lambda (alist a)
	    (if alist (princ #\Tab stream))
	    (if (or (null (slot-value tk a))
		    (equal "" (slot-value tk a)))
		(princ "_" stream)
		(princ (slot-value tk a) stream))
	    (append alist (cons a nil)))
	  '(id form lemma upostag xpostag feats head deprel deps misc)
	  :initial-value nil))


(defun write-mtoken (mtk stream)
  (reduce (lambda (alist a)
	    (if alist (princ #\Tab stream))
	    (princ a stream)
	    (cons a alist))
	  (append (list (format nil "~a-~a" (mtoken-start mtk) (mtoken-end mtk))
			(mtoken-form mtk))
		  (make-list 7 :initial-element '_ )
		  (list (mtoken-misc mtk)))
	  :initial-value nil))


(defun write-sentence (sentence stream)
  (mapcar (lambda (pair)
            (if (equal :raw (car pair))
		(format stream "# ~a~%" (cdr pair))
                (format stream "# ~a = ~a~%" (car pair) (cdr pair))))
	  (sentence-meta sentence))
  (with-slots (tokens mtokens) sentence
    (setf mtokens (sort mtokens #'<= :key #'mtoken-start))
    (reduce (lambda (alist tk)
	      (let* ((next-mtoken (find-if (lambda (x) (>= x (token-id tk)))
					   mtokens
					   :key 'mtoken-start)))
		(if alist (princ #\Linefeed stream))
		(when (and next-mtoken
			   (equal (mtoken-start next-mtoken) (token-id tk)))
		  (write-mtoken next-mtoken stream)
		  (princ #\Newline stream))
		(write-token tk stream)
		(cons tk alist)))
	    tokens :initial-value nil))
  (princ #\Newline stream))


(defun write-conllu-to-stream (sentences &optional (out *standard-output*))
  (reduce (lambda (alist sent)
	    (if alist (princ #\Newline out))
	    (write-sentence sent out)
	    (cons sent alist))
	  sentences :initial-value nil)
  (princ #\Newline out)
  (values))


(defun write-conllu (sentences filename &key (if-exists :supersede))
  (with-open-file (out filename :direction :output :if-exists if-exists)
    (write-conllu-to-stream sentences out)))



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
		    (split " " line)))
	   sentences))))))

(defun read-file-tag-suffix (filename &key (tag 'upostag) (separator "_"))
  (with-open-file (input filename :direction :input)
    (read-sentence-tag-suffix input tag separator)))
