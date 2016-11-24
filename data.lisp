
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


(defun sentence->text (sentence)
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (tk) (slot-value tk 'form))
		  (sentence-tokens sentence))))


(defun sentence-size (sentence)
  (length (sentence-tokens sentence)))

