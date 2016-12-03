
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


(defun collect-meta (lines)
  (mapcar (lambda (line)
	    (let* ((cl  (string-trim '(#\# #\Space #\Tab) line))
		   (pos (position #\Space cl)))
	      (cons (subseq cl 0 pos)
		    (subseq cl (1+ pos)))))
	  lines))


(defun make-sentence (lineno lines fn-meta)
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
      (make-instance 'sentence :start lineno :tokens tokens :meta (funcall fn-meta meta)))))


(defun read-conllu (filename &key (fn-meta #'collect-meta))
  (with-open-file (in filename)
    (read-conllu-from-stream in :fn-meta fn-meta)))


(defun read-conllu-from-stream (stream &key (fn-meta #'collect-meta))
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
	    (princ (slot-value tk a) stream)
	    (append alist (cons a nil)))
	  '(id form lemma upostag xpostag feats head deprel deps misc)
	  :initial-value nil))


(defun write-sentence (sentence stream)
  (mapcar (lambda (pair)
	    (format stream "# ~a ~a~%" (car pair) (cdr pair)))
	  (sentence-meta sentence))
  (reduce (lambda (alist tk)
	    (if alist (princ #\Linefeed stream))
	    (write-token tk stream)
	    (append alist (cons tk nil)))
	  (sentence-tokens sentence) :initial-value nil)
  (princ #\Linefeed stream)) 


(defun write-conllu-to-stream (sentences out)
  (reduce (lambda (alist sent)
	    (if alist (princ #\Linefeed out))
	    (write-sentence sent out)
	    (append alist (cons sent nil)))
	  sentences :initial-value nil))


(defun write-conllu (sentences filename &key (if-exists :supersede))
  (with-open-file (out filename :direction :output :if-exists if-exists)
    (write-conllu-to-stream sentences out)))

