
(in-package :cl-conllu)


(defclass token ()
  ((id      :initarg :id
	    :accessor token-id)
   (form    :initarg :form
	    :accessor token-form)
   (lemma   :initarg :lemma
	    :accessor token-lemma)
   (upostag :initarg :upostag
	    :accessor token-upostag)
   (xpostag :initarg :xpostag
	    :accessor token-xpostag)
   (feats   :initarg :feats
	    :accessor token-feats)
   (head    :initarg :head
	    :accessor token-head)
   (deprel  :initarg :deprel
	    :accessor token-deprel)
   (deps    :initarg :deps
	    :accessor token-deps)
   (misc    :initarg :misc
	    :accessor token-misc)))

(defclass mtoken ()
  ((start   :initarg :start
	    :accessor mtoken-start)
   (end     :initarg :end
	    :accessor mtoken-end)
   (form    :initarg :form
	    :accessor mtoken-form)))

(defclass sentence ()
  ((start   :initarg :start
	    :initform 0
	    :accessor sentence-start)
   (meta    :initarg :meta
	    :initform nil
	    :accessor sentence-meta)
   (tokens  :initarg :tokens
	    :initform nil
	    :accessor sentence-tokens)
   (mtokens :initarg :mtokens
	    :initform nil
	    :accessor sentence-mtokens)))


(defun sentence-meta-value (sentence meta-field)
  (cdr (assoc meta-field (sentence-meta sentence) :test #'equal)))

(defun sentence->text (sentence)
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (tk) (slot-value tk 'form))
		  (sentence-tokens sentence))))


(defun sentence-size (sentence)
  (length (sentence-tokens sentence)))


(defun sentence->deep (sentence &key fn-key)
  (labels ((ensure-list (key)
	     (if (symbolp key) (list fn-key) key)))
    (if (functionp fn-key)
	(deep-aux (sentence-root sentence) sentence fn-key)
	(if (or (symbolp fn-key)
		(listp fn-key))
	    (deep-aux (sentence-root sentence) sentence
		      (lambda (tk)
			(let ((out (loop for k in (ensure-list fn-key)
					 collect (slot-value tk k))))
			  (if (and (listp out) (= 1 (length out)))
			      (car out) out))))))))

(defun deep-aux (root sentence fn-key)
  (list (funcall fn-key root)
	(loop for child in (token-child root sentence)
	      collect (list (slot-value child 'deprel)
			    (if (token-child child sentence)
				(deep-aux child sentence fn-key)
				(funcall fn-key child))))))

(defun sentence-root (sentence)
  (car (remove-if-not (lambda (tk) (equal "0" (slot-value tk 'head)))
		      (sentence-tokens sentence))))

(defun token-child (token sentence)
  (remove-if-not (lambda (tk)
		   (equal (slot-value tk 'head) (slot-value token 'id)))
		 (sentence-tokens sentence)))
