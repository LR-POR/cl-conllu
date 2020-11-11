(in-package :cl-conllu)

(define-condition malformed-field (error) 
  ((line  :initarg :line)
   (field :initarg :field)
   (value :initarg :value))
  (:report (lambda (c s)
	     (with-slots (line field value) c
	       (format s "Invalid ~s for ~s in line ~s" value field line)))))

(define-condition malformed-line (error) 
  ((line :initarg :line)
   (message :initarg :message))
  (:report (lambda (condition stream)
	     (with-slots (message line) condition
	       (format stream message line)))))


(defun parse-field (line field value)
  (handler-case (parse-integer value)
    (parse-error ()
      (error 'malformed-field :line line :field field :value value))))


(defun line->token (line lineno)
  (let ((fields (cl-ppcre:split "\\t" line)))
    (if (not (= 10 (length fields)))
	(error 'malformed-line :message "Line ~s has less than 10 fields"
			       :line line)
	(destructuring-bind (id form lemma upostag xpostag feats head deprel deps misc)
	    fields
	  (optima:match id
	    ((optima.ppcre:ppcre "^(\\d+)-(\\d+)$" begin end)
	     (make-instance 'mtoken :lineno lineno
				    :start  (parse-field line 'start begin)
				    :end    (parse-field line 'end end)
				    :form form :misc misc))
	    ((optima.ppcre:ppcre "^(\\d+)\\.(\\d+)$" prev idx)
	     (make-instance 'etoken :lineno lineno
				    :prev   (parse-field line 'prev  prev)
				    :index  (parse-field line 'index idx) 
				    :form   form :lemma lemma
				    :upostag upostag :xpostag xpostag :feats feats
				    :deps    deps :misc misc))
	    ((optima.ppcre:ppcre "^(\\d+)$" idx)
	     (make-instance 'token  :lineno lineno
				    :id   (parse-field line 'id idx)
				    :form form :lemma lemma
				    :upostag upostag :xpostag xpostag :feats feats
				    :head (parse-field line 'head head) :deprel deprel
				    :deps deps :misc misc))
	    (other (error 'malformed-field :line line
					   :field 'id :value other)))))))



(defun collect-meta (lines)
  (mapcar (lambda (line)
	    (let* ((cl (string-right-trim '(#\Space #\Tab) (string-left-trim '(#\# #\Space) line)))
		   (pos (position #\= cl)))
              (if pos
                  (cons (subseq cl 0 (1- pos))
                        (string-left-trim '(#\Space) (subseq cl (1+ pos))))
                  (cons cl :none))))
	  lines))


(defun make-sentence (lineno lines fn-meta)
  (let (meta tokens mtokens etokens)
    (handler-case
	(loop for line in lines
	      for cline = lineno then (incf cline)
	      do (if (equal #\# (char line 0))
		     (push line meta)
		     (let ((tk (line->token line cline)))
		       (ecase (type-of tk)
			 (token  (push tk tokens))
			 (mtoken (push tk mtokens))
			 (etoken (push tk etokens)))))
	      finally
		 (return (make-instance 'sentence :start   lineno
						  :meta    (funcall fn-meta (reverse meta))
						  :tokens  (reverse tokens)
						  :mtokens (reverse mtokens)
						  :etokens (reverse etokens))))
      (malformed-line (e) nil)
      (malformed-field (e) nil))))


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

(defun lazy-stream-reader (stream &key (fn-meta #'collect-meta) (start-lineno 0))
  "Return a function that returns one CoNLL-U sentence per call. "
  (let ((curr-lineno start-lineno))
    (lambda ()
      (multiple-value-bind (sent lineno)
          (read-stream stream :fn-meta fn-meta :stop? #'blank-line?
                       :start-lineno curr-lineno)
        (setf curr-lineno lineno)
        (car sent)))))


(defun read-stream (stream &key (fn-meta #'collect-meta) (stop? #'null) (start-lineno 0))
  (macrolet ((flush-line ()
	       `(setq line (read-line stream nil nil)
		      lineno (1+ lineno))))
    (prog (line (lineno start-lineno) beginning lines sentences)
     label-1
     (flush-line)
     (let ((blank-line? (blank-line? line)))
       (cond
         ((and line blank-line? (not lines)) (go label-1))
         ((or (null line) (funcall stop? line))  (go label-3))
         (blank-line? (go label-1))
         (t (setq beginning lineno)
	    (push line lines)
	    (go label-2))))
     
     label-2
     (flush-line)
     (cond ((or (null line) (funcall stop? line)) (go label-3))
	   ((equal line "") (push (make-sentence beginning (reverse lines) fn-meta)
				  sentences)
	    (setq lines nil)
	    (go label-1))
	   (t (push line lines)
	      (go label-2)))
     
     label-3
     (if lines
	 (push (make-sentence beginning (reverse lines) fn-meta)
	       sentences))
     (return (values (reverse sentences) lineno)))))


(defgeneric write-token (abstract-token stream)
  (:documentation "write a token to a line in 10 tab-separated columns."))

(defmethod write-token ((tk token) stream)
  (reduce (lambda (alist a)
	    (if alist (princ #\Tab stream))
	    (if (or (null (slot-value tk a))
		    (equal "" (slot-value tk a)))
		(princ "_" stream)
		(princ (slot-value tk a) stream))
	    (append alist (cons a nil)))
	  '(id form lemma upostag xpostag feats head deprel deps misc)
	  :initial-value nil))

(defmethod write-token ((tk mtoken) stream)
  (reduce (lambda (alist a)
	    (if alist (princ #\Tab stream))
	    (case a
	      ('start-end
	       (princ (format nil "~a-~a" (mtoken-start tk) (mtoken-end tk)) stream))
	      (:none
	       (princ "_" stream))
	      (t (if (or (null (slot-value tk a))
			 (equal "" (slot-value tk a)))
		     (princ "_" stream)
		     (princ (slot-value tk a) stream))))
	    (append alist (cons a nil)))
	  '(start-end form :none :none :none :none :none :none :none misc)
	  :initial-value nil))

(defmethod write-token ((tk etoken) stream)
  (reduce (lambda (alist a)
	    (if alist (princ #\Tab stream))
	    (case a
	      ('prev-index
	       (princ (format nil "~a.~a" (etoken-prev tk) (etoken-index tk)) stream))
	      (:none
	       (princ "_" stream))
	      (t (if (or (null (slot-value tk a))
			 (equal "" (slot-value tk a)))
		     (princ "_" stream)
		     (princ (slot-value tk a) stream))))
	    (append alist (cons a nil)))
	  '(prev-index form lemma upostag xpostag feats :none :none deps misc)
	  :initial-value nil))


(defun write-sentence (sentence stream)
  (mapcar (lambda (pair)
            (if (equal :none (cdr pair))
		(format stream "# ~a~%" (car pair))
                (format stream "# ~a = ~a~%" (car pair) (cdr pair))))
	  (sentence-meta sentence))
  (with-slots (tokens mtokens etokens) sentence
    (let ((all-tokens (sort (copy-seq (append tokens mtokens etokens)) #'<= :key #'token-lineno)))
      (reduce (lambda (alist tk)
	      (if alist (princ #\Linefeed stream))
	      (write-token tk stream)
	      (cons tk alist))
	    all-tokens :initial-value nil)))
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


;; JSON output

(defmethod yason:encode ((sentence sentence) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "text"    (sentence-text sentence))
      (yason:encode-object-element "sent_id" (sentence-id sentence))
      (yason:with-object-element ("tokens")
	(yason:encode-array-elements (sentence-tokens sentence))))))


(defmethod yason:encode ((token token) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (loop for k in '(id form lemma upostag xpostag feats head deprel deps misc cfrom cto lineno)
	    do (yason:encode-object-element (string-downcase (symbol-name k))
					    (slot-value token k))))))
