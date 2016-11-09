
(in-package :cl-conllu)


(defclass token ()
  (id form lemma upostag xpostag feats head deprel deps misc))


(defclass sentence ()
  ((start  :initarg :start
	   :initform 0
	   :accessor sentence-start)
   (meta   :initarg :meta
	   :initform nil
	   :accessor sentence-meta)
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


(defun line->values (line)
  (if (cl-ppcre:scan "^#" line)
      (let ((data (cl-ppcre:split "[ \\t]+" line)))
	(values (cadr data) (caddr data)))
      line))


(defun collect-meta (lines)
  (let ((meta (make-hash-table :test #'equal)))
    (mapc (lambda (line)
	    (multiple-value-bind (k v)
		(line->values line)
	      (setf (gethash k meta) v)))
	  lines)
    meta))


(defun make-sentence (lineno lines)
  (labels ((reading (lines meta tokens)
	     (cond
	       ((null lines)
		(values (reverse meta) (reverse tokens)))
	       ((cl-ppcre:scan "^#" (car lines))
		(reading (cdr lines) (cons (car lines) meta) tokens))
	       ((cl-ppcre:scan "^[1-9]+" (car lines))
		(reading (cdr lines) meta (cons (line->token (car lines)) tokens))))))
    (multiple-value-bind (meta tokens)
	(reading lines nil nil)
      (make-instance 'sentence :start lineno :tokens tokens :meta (collect-meta meta)))))


(defun read-conllu (filename)
  (macrolet ((flush-line ()
	       `(setq line (read-line in nil nil)
		      lineno (+ lineno 1))))
    (with-open-file (in filename)
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
	 ("" (push (make-sentence begining (reverse lines))
		   sentences)
	     (setq lines nil)
	     (go label-1))
	 (t (push line lines)
	    (go label-2)))

       label-3
       (return (reverse sentences))))))


(defun write-conllu (sentences)
  "...")


(defun sentence->text (sentence)
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (tk) (slot-value tk 'form))
		  (sentence-tokens sentence))))

