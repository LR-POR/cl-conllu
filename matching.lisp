
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

(defun read-file (filename)
  (with-open-file (in filename)
    (do ((line (read-line in nil nil)
	       (read-line in nil nil))
	 (lineno 0
		 (+ 1 lineno))
	 (current (make-instance 'sentence))
	 (groups nil))
	((null line)
	 (progn (push current groups)
		(reverse groups)))
      (if (equal line "")
	  (progn (push current groups)
		 (setf current (make-instance 'sentence))
		 (setf (sentence-start current) (+ 1 lineno)))
	  (push line (sentence-tokens current))))))


(defun read-file (filename)
  (with-open-file (in filename)
    (do ((line (read-line in nil nil)
	       (read-line in nil nil))
	 (lineno 0
		 (+ 1 lineno))
	 (state 1)
	 (tokens nil)
	 (begining 0)
	 (sentences nil))
	((null line)
	 (push (reverse tokens) sentences)
	 (reverse sentences))
      (cond
	((and (equal state 1) (equal line ""))
	 nil)
	((and (equal state 1) (not (equal line "")))
	 (setf state 2
	       begining lineno)
	 (push line tokens))
	((and (equal state 2) (not (equal line "")))
	 (push line tokens))
	((and (equal state 2) (equal line ""))
	 (push (make-instance 'sentence :start begining :tokens (reverse tokens))
	       sentences)
	 (setf state 1
	       tokens nil))))))



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


(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

;; (defparameter *sentences* (cl-ppcre:split "\\n\\n" (file-string "release/pt-ud-dev.conllu")))

(defun words (sentence)
  (mapcar (lambda (s) (cl-ppcre:split "\\t" s))
	  (remove-if (lambda (s) (cl-ppcre:scan "^#" s)) sentence)))


;; (format nil "~{~a~^ ~}" (mapcar #'second (words (cl-ppcre:split "\\n" (car *sentences*)))))

