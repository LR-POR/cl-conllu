
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
       (if lines
	   (push (make-sentence begining (reverse lines))
		 sentences))
       (return (reverse sentences))))))


(defun list-to-tsv (alist)
  (format nil "~{~a~^~a~}"
	  (reduce (lambda (a alist)
		    (if alist
			(cons a (cons #\Tab alist))
			(cons a alist)))
		  alist :from-end t :initial-value nil)))


(defun write-conllu (sentences filename &key (if-exists :supersede))
  (labels ((print-cols (slots obj out start)
	     (if (null slots)
		 (princ #\Linefeed out)
		 (progn
		   (if (not start)
		       (princ #\Tab out))
		   (princ (slot-value obj (car slots)) out)
		   (print-cols (cdr slots) obj out nil)))))
    (with-open-file (out filename :direction :output :if-exists if-exists)
      (let ((start t))
	(dolist (sent sentences)
	  (if start
	      (setq start nil)
	      (princ #\Linefeed out))
	  (maphash (lambda (k v)
		     (format out "# ~a ~a~%" k v))
		   (sentence-meta sent))
	  (mapc (lambda (tk)
		  (print-cols '(id form lemma upostag xpostag feats head deprel deps misc)
			      tk out t))
		(sentence-tokens sent)))))))


(defun sentence->text (sentence)
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (tk) (slot-value tk 'form))
		  (sentence-tokens sentence))))

