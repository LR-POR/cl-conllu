
(in-package :conllu-palavras)

;; Links:
;; http://beta.visl.sdu.dk/cg3/chunked/streamformats.html#stream-niceline
;; http://weitz.de/cl-ppcre/

;; one of PALAVRAS stream format is the niceline. This code transform
;; the niceline format into CoNLL-U format. We don't change the of POS
;; or deprels tags. We don't assume any specific order for the tags:
;;
;; $?FORM \t [FORM] <NAME-WITH-SPACE>* NAME+ @NAME+ §NAME* #NUM->NUM
;; $FORM NAME @NAME #NUM->NUM
;;
;; We still need many improvements: (1) error handler; (2) sentences
;; metadata.

(eval-when (eval compile)
    (deflexer niceline
	:flex-compatible
      ("\\[([^ ]+)\\]"   (return (cons :lemma %1)))
      ("<[^>]+>"         (return (cons   :sem (substitute #\_ #\Space %0))))
      ("@[^ ]+"          (return (cons  :func %0)))
      ("§[^ ]+"          (return (cons  :role %0)))
      ("£[^ ]+"          (return (cons :extra %0)))
      ("[^:space:]+"     (return (cons   :pos %0)))
      ("#([0-9]+)->([0-9]+)" (return (cons :link (cons %1 %2))))
      ("[:space:]+")))


(defun add-field-value (token field value)
  (if (string= (slot-value token field) "_")
      (setf (slot-value token field) value)
      (setf (slot-value token field)
	    (format nil "~a|~a" (slot-value token field) value)))
  token)


(defun consume-tags (tags tk)
  (let ((lex (niceline tags)))
    (loop for pair = (funcall lex)
	  with pos = nil
	  while pair
	  do (cond ((equal (car pair) :lemma)
		    (add-field-value tk 'lemma (cdr pair)))
		   ((and (not pos) (equal (car pair) :pos))
		    (add-field-value tk 'upostag (cdr pair))
		    (setf pos t))
		   ((and pos (equal (car pair) :pos))
		    (add-field-value tk 'feats (cdr pair)))
		   ((equal (car pair) :func)
		    (add-field-value tk 'deprel (cdr pair)))
		   ((member (car pair) '(:extra :sem :role))
		    (add-field-value tk 'xpostag (cdr pair)))
		   ((equal (car pair) :link)
		    (setf (slot-value tk 'cl-conllu::id)   (car (cdr pair))
			  (slot-value tk 'cl-conllu::head) (cdr (cdr pair))))))
    tk))


(define-condition malformed-line-error (error)
  ((text :initarg :text :reader malformed-line)))


(defun read-niceline (line)
  (cond ((cl-ppcre:scan "\\t" line)
	 (destructuring-bind (form rest)
	     (subseq (cl-ppcre:split "[ ]*\\t[ ]*" line) 0 2)
	   (let ((tk (make-instance 'cl-conllu:token
				    :id 0
				    :form (if (cl-ppcre:scan "^\\$" form) (subseq form 1) form)
				    :lemma "_")))
	     (consume-tags rest tk))))
	((cl-ppcre:scan "^\\$[^ ]+[ ]+PU" line)
	 (multiple-value-bind (a b c d)
	     (cl-ppcre:scan "^\\$([^ ]+)[ ]+PU" line)
	   (declare (ignore a b))
	   (let ((tk (make-instance 'cl-conllu:token
				    :id 0
				    :form (subseq line (aref c 0) (aref d 0))
				    :lemma (subseq line (aref c 0) (aref d 0)))))
	     (consume-tags (subseq line (1+ (aref d 0))) tk))))
	(t (error 'malformed-line-error :text line))))


(defun read-niceline-stream (stream)
  (let (sentences tokens)
    (do ((line (read-line stream nil nil)
	       (read-line stream nil nil)))
	((null line)
	 (progn
	   (when tokens
	     (push (make-instance 'sentence :tokens (reverse tokens)) sentences))
	   (reverse sentences)))
      (cond ((equal line "") 
	     (when tokens
	       (push (make-instance 'sentence :tokens (reverse tokens)) sentences)
	       (setf tokens nil)))
	    (t (push (read-niceline line) tokens))))))


(defun read-niceline-file (filename)
  (with-open-file (stream filename)
    (handler-case (read-niceline-stream stream)
      (malformed-line-error (v)
	(format t "~%ERROR in ~a ~a~%" filename (malformed-line v))
	nil))))


(defun convert-directory (path)
  (mapc (lambda (file)
	  (let ((sents (read-niceline-file file)))
	    (if sents (write-conllu sents (make-pathname :type "conllu" :defaults file)))))			 
	(directory path)))
