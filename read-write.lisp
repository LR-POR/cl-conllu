
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
	(setf (slot-value mtk 'start) (parse-integer (aref range 0)))
	(setf (slot-value mtk 'end) (parse-integer (aref range 1)))
	(setf (slot-value mtk 'form) (cadr res))
	mtk)))


(defun collect-meta (lines)
  (mapcar (lambda (line)
	    (let* ((cl  (string-trim '(#\# #\Space #\Tab) line))
		   (pos (position #\Space cl)))
	      (cons (subseq cl 0 pos)
		    (subseq cl (1+ pos)))))
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


(defun write-mtoken (mtk stream)
  (reduce (lambda (alist a)
	    (if alist (princ #\Tab stream))
	    (princ a stream)
	    (cons a alist))
	  (append (list (format nil "~a-~a" (mtoken-start mtk) (mtoken-end mtk))
			(mtoken-form mtk))
		  (make-list 8 :initial-element '_ ))
	  :initial-value nil))


(defun write-sentence (sentence stream)
  (mapcar (lambda (pair)
	    (format stream "# ~a ~a~%" (car pair) (cdr pair)))
	  (sentence-meta sentence))
  (reduce (lambda (alist tk)
	    (let* ((next-mtoken (find-if (lambda (x) (>= x (token-id tk)))
					 (sentence-mtokens sentence)
					 :key 'mtoken-start)))
	      (if alist (princ #\Linefeed stream))
	      (when (and next-mtoken
			 (eq (mtoken-start next-mtoken) (token-id tk)))
		(write-mtoken next-mtoken stream)
		(princ #\Newline stream))
	      (write-token tk stream)
	      (cons tk alist)))
	  (sentence-tokens sentence) :initial-value nil)
  (princ #\Newline stream))


(defun write-conllu-to-stream (sentences out)
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
