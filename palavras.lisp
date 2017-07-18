
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


(defparameter niceline '((lemma   "\\[(.*)\\]" xpostag error)
			 (xpostag "<([^>]*)>" xpostag upostag)
			 (upostag "([^ ]+)" feats error)
			 (feats   "^[^[<#@§]([^ ]+)" )
			 (deprel  "@([^ ]+)" nil)
			 (xpostag "§([^ ]+)" nil)
			 (link    "#([0-9]+)->([0-9]+)" nil)))


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


(defun read-niceline (line)
  (cond ((cl-ppcre:scan "[ ]*\\t[ ]*" line)
	 (destructuring-bind (form rest)
	     (cl-ppcre:split "[ ]*\\t[ ]*" line)
	   (let ((tags (cl-ppcre:split "[ ]+" rest))
		 (tk  (make-instance 'cl-conllu:token
				     :id 0
				     :form (if (cl-ppcre:scan "^\\$" form) (subseq form 1) form)
				     :lemma "_")))
	     (consume-tags tags tk))))
	((cl-ppcre:scan "^\\$.[ ]+PU" line)
	 (let ((tags (cl-ppcre:split "[ ]+" line)))
	   (multiple-value-bind (a b)
	       (cl-ppcre:scan-to-strings "#([0-9]+)->([0-9]+)" (nth 3 tags))
	     (declare (ignore a))
	     (make-instance 'cl-conllu:token
				       :id (aref b 0)
				       :head (aref b 1)
				       :form (subseq (car tags) 1)
				       :upostag "PU"
				       :deprel "PU"
				       :lemma (subseq (car tags) 1)))))
	((equal 0 (length line))
	 nil)
	(t (error "ill-formed input."))))


(defun read-niceline-stream (stream)
  (let (sentences tokens)
    (do ((line (read-line stream nil nil)
	       (read-line stream nil nil)))
	((null line)
	 (reverse sentences))
      (cond ((equal line "<ß>")
	     (setf tokens nil))
	    ((equal line "</ß>")
	     (push (make-instance 'sentence :tokens (reverse tokens)) sentences))
	    (t (let ((tk (read-niceline line)))
		 (if tk (push tk tokens))))))))


(defun read-niceline-file (filename)
  (with-open-file (stream filename)
    (read-niceline-stream stream)))
