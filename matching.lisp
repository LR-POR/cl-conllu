
(in-package :cl-conllu)


(defclass token ()
  (id form lemma upostag xpostag feats head deprel deps misc))

(defclass sentence ()
  ((start  :initarg :start
	   :initform 0
	   :accessor sentence-start)
   (tokens :initarg :tokens
	   :initform nil
	   :accessor sentence-tokens)))

(defun line->token (line)
  (if (cl-ppcre:scan "^#" line)
      line
      (let ((tk (make-instance 'token)))
	(assert (equal 10 (length (cl-ppcre:split "\\t" line))))
	(mapc (lambda (value key)
		(setf (slot-value tk key) value))
	      (cl-ppcre:split "\\t" line)
	      '(id form lemma upostag xpostag feats head deprel deps misc))
	tk)))

(defun read-file (filename)
  (macrolet ((flush-line ()
	       `(setq line (read-line in nil nil)
		      lineno (+ lineno 1))))
    (with-open-file (in filename)
      (prog (line begining (lineno 0) tokens sentences)
       label-1
       (flush-line)
       (alexandria:switch (line :test #'equal)
	 (nil (go label-3))
	 ("" (go label-1))
	 (t (setq begining lineno)
	    (push (line->token line) tokens)
	    (go label-2)))
     
       label-2
       (flush-line)
       (alexandria:switch (line :test #'equal)
	 (nil (go label-3))
	 ("" (push (make-instance 'sentence :start begining :tokens (reverse tokens))
		   sentences)
	     (setq tokens nil)
	     (go label-1))
	 (t (push (line->token line) tokens)
	    (go label-2)))

       label-3
       (return (reverse sentences))))))


(defun sentence->text (sentence)
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (tk) (slot-value tk 'lemma))
		  (remove-if-not (lambda (tk)
				   (equal (class-of tk)
					  (find-class 'token)))
				 (sentence-tokens sentence)))))



