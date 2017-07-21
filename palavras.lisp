
(in-package :conllu-palavras)

;; Links:
;; http://beta.visl.sdu.dk/cg3/chunked/streamformats.html#stream-niceline
;; http://weitz.de/cl-ppcre/

;; one of PALAVRAS stream format is the niceline. This code transform
;; the niceline format into CoNLL-U format. We don't change the of POS
;; or deprels tags. We are assuming an order of tags:
;;
;; $?FORM \t [FORM] <NAME-WITH-SPACE>* NAME+ @NAME+ §NAME* #NUM->NUM
;; $FORM NAME @NAME #NUM->NUM

(eval-when (eval)
    (deflexer niceline
	:flex-compatible
      ("\\[[^ ]+\\]"     (return (list :lemma %0)))
      ("<[^>]+>"         (return (list   :sem %0)))
      ("@[^ ]+"          (return (list  :func %0)))
      ("§[^ ]+"          (return (list  :role %0)))
      ("£[^ ]+"          (return (list :extra %0)))
      ("[^:space:]+"     (return (list   :pos %0)))
      ("#[0-9]+->[0-9]+" (return (list  :link %0)))
      ("[:space:]+")))


(define-condition malformed-line-error (error)
  ((text :initarg :text :reader text)))



(defun add-field-value (token field value)
  (format t "~a ~a~%" field value)
  (if (string= (slot-value token field) "_")
      (setf (slot-value token field) value)
      (setf (slot-value token field)
	    (format nil "~a|~a" (slot-value token field) value)))
  token)


(defun consume-tags (tags token)
  (macrolet ((next-tag (field value)
	       `(progn
		  (add-field-value token ,field ,value)
		  (setq current (car rest)
			rest (cdr rest)))))
    (prog ((current (car tags)) (rest (cdr tags)))

     label-1
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "\\[(.*)\\]" current)
       (if a
	   (progn
	     (next-tag 'lemma (aref b 0))
	     (go label-2))
	   (error "ill-formed input in 1.")))

     label-2
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "<([^>]*)>" current)
       (if a
	   (progn
	     (next-tag 'xpostag (aref b 0))
	     (go label-2))
	   (go label-3)))

     label-3
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "^([^<#@§][^ ]*)" current)
       (if a
	   (progn
	     (next-tag 'upostag (aref b 0))
	     (go label-4))
	   (error "ill-formed input in 3.")))

     label-4
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "^([^<#@§][^ ]*)" current)
       (if a
	   (progn
	     (next-tag 'feats (aref b 0))
	     (go label-4))
	   (go label-5)))

     label-5
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "@([^ ]+)" current)
       (if a
	   (progn
	     (next-tag 'deprel (aref b 0))
	     (go label-5))
	   (go label-6)))

     label-6
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "§([^ ]+)" current)
       (if a
	   (progn
	     (next-tag 'xpostag (aref b 0))
	     (go label-6))
	   (go label-7)))

     label-7
     (multiple-value-bind (a b)
	 (cl-ppcre:scan-to-strings "#([0-9]+)->([0-9]+)" current)
       (if a
	   (progn
	     (setf (slot-value token 'CL-CONLLU::ID)   (aref b 0)
		   (slot-value token 'CL-CONLLU::HEAD) (aref b 1)))
	   (error "ill-formed input in 7."))
       (return token)))))


(defun consume-tags (tags)
  (let ((lex (niceline tags)))
    (loop for pair = (funcall lex)
	  while pair
	  collect pair)))


(defun read-niceline (line)
  (cond ((cl-ppcre:scan "\\t" line)
	 (destructuring-bind (form rest)
	     (subseq (cl-ppcre:split "\\t" line) 0 2)
	   (let ((tk (make-instance 'cl-conllu:token
				    :id 0
				    :form (string-trim '(#\Space) (if (cl-ppcre:scan "^\\$" form) (subseq form 1) form))
				    :lemma "_")))
	     (declare (ignore tk))
	     (consume-tags rest))))
	((cl-ppcre:scan "^\\$([^ ]+)[ ]+PU" line)
	 (multiple-value-bind (a b c d)
	     (cl-ppcre:scan "^\\$([^ ]+)[ ]+PU" line)
	   (declare (ignore a b))
	   (let ((tk (make-instance 'cl-conllu:token
				    :id 0
				    :form (subseq line (aref c 0) (aref d 0))
				    :lemma "_")))
	     (declare (ignore tk))
	     (consume-tags (subseq line (1+ (aref d 0)))))))
	((equal 0 (length line))
	 nil)
	(t (error "ill-formed input."))))


(defun read-niceline-stream (stream)
  (let (sentences tokens)
    (do ((line (read-line stream nil nil)
	       (read-line stream nil nil)))
	((null line)
	 (reverse sentences))
      (cond ((equal line "") 
	     (when tokens
	       (push (make-instance 'sentence :tokens (reverse tokens)) sentences)
	       (setf tokens nil)))
	    (t (let ((tk (read-niceline line)))
		 (if tk (push tk tokens))))))))


(defun read-niceline-file (filename)
  (with-open-file (stream filename)
    (read-niceline-stream stream)))
