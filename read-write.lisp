
(in-package :cl-conllu)

(defun line->token (line pos)
  (if (cl-ppcre:scan "^#" line)
      line
      (let ((tk     (make-instance 'token))
	    (fields (cl-ppcre:split "\\t" line)))
	(assert (equal 10 (length fields)))
	(mapc (lambda (value key)
		(setf (slot-value tk key)
		      (case key
			(id   (parse-integer value))
			(head (if (string-equal value "_") nil
				  (parse-integer value)))
			(t    value))))
	      fields
	      '(id form lemma upostag xpostag feats head deprel deps misc))
	(setf (slot-value tk 'lineno) pos)
	tk)))


(defun line->etoken (line pos)
  (if (cl-ppcre:scan "^#" line)
      line
      (let ((fields (cl-ppcre:split "\\t" line)))
	(assert (equal 10 (length fields)))
	(register-groups-bind (prev idx) 
	    ("([0-9]+)\\.([0-9]+)" (nth 0 fields) :sharedp t)
	  (make-instance 'etoken
		       :prev    prev
		       :index   idx
		       :form    (nth 1 fields)
		       :lemma   (nth 2 fields)
		       :upostag (nth 3 fields)
		       :xpostag (nth 4 fields)
		       :feats   (nth 5 fields)
		       :deps    (nth 8 fields)
		       :misc    (nth 9 fields)
		       :lineno  pos)))))


(defun line->mtoken (line pos)
  (if (cl-ppcre:scan "^#" line)
      line
      (let* ((mtk (make-instance 'mtoken))
	     (res (cl-ppcre:split "\\t" line))
	     (range (cadr (multiple-value-list (cl-ppcre:scan-to-strings "([0-9]+)-([0-9]+)" (car res))))))
	(assert (equal 10 (length res)))
	(setf (slot-value mtk 'start)  (parse-integer (aref range 0))
	      (slot-value mtk 'end)    (parse-integer (aref range 1))
	      (slot-value mtk 'form)   (second res)
	      (slot-value mtk 'misc)   (car (last res))
	      (slot-value mtk 'lineno) pos)
	mtk)))

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
  (labels ((reading (lines meta tokens mtokens etokens pos)
	     (cond
	       ((null lines)
		(values (reverse meta) (reverse tokens) (reverse mtokens) (reverse etokens)))
	       ((cl-ppcre:scan "^#" (car lines))
		(reading (cdr lines) (cons (car lines) meta) tokens mtokens etokens pos))
	       ;; range for multiword tokens
	       ((cl-ppcre:scan "^[0-9]+-[0-9]+\\t" (car lines))
		(reading (cdr lines) meta tokens (cons (line->mtoken (car lines) pos) mtokens) etokens (incf pos)))
	       ;; normal tokens
	       ((cl-ppcre:scan "^[0-9]+\\t" (car lines))
		(reading (cdr lines) meta (cons (line->token (car lines) pos) tokens) mtokens etokens (incf pos)))
	       ;; empty nodes in enhanced dependencies
	       ((cl-ppcre:scan "^[0-9]+.[0-9]+\\t" (car lines))
		(reading (cdr lines) meta tokens mtokens (cons (line->etoken (car lines) pos) etokens) (incf pos))))))
    (multiple-value-bind (meta tokens mtokens etokens)
	(reading lines nil nil nil nil 0)
      (make-instance 'sentence :start lineno :tokens tokens
			       :meta (funcall fn-meta meta)
			       :mtokens mtokens
			       :etokens etokens))))


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


;; O(2n) complexity
(defun list-to-tsv (alist stream)
  (format stream "~{~a~^~a~}"
	  (reduce (lambda (a alist)
		    (if alist
			(cons a (cons #\Tab alist))
			(cons a alist)))
		  alist :from-end t :initial-value nil)))



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
    (setf all-tokens (sort (append tokens mtokens etokens) #'<= :key #'token-lineno))
    (reduce (lambda (alist tk)
	      (if alist (princ #\Linefeed stream))
	      (write-token tk stream)
	      (cons tk alist))
	    all-tokens :initial-value nil))
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
      (yason:encode-object-element "text"
				   (sentence-meta-value sentence "text"))
      (yason:encode-object-element "sent_id"
				   (sentence-meta-value sentence "sent_id"))
      (yason:with-object-element ("tokens")
	(yason:encode-array-elements (sentence-tokens sentence))))))


(defmethod yason:encode ((token token) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (loop for k in '(id form lemma upostag xpostag feats head deprel deps misc)
	    do (yason:encode-object-element (symbol-name k)
					    (slot-value token k))))))
