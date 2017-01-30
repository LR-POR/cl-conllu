
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
	    :accessor mtoken-form)
   (misc    :initarg :misc
	    :initform "_"
	    :accessor mtoken-misc)))

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


(defun sentence-text-aux (tokens mtokens garbage-end response)
  (labels ((forma (obj)
	     (if (search "SpaceAfter=No" (slot-value obj 'misc))
		 (list (slot-value obj 'form))
		 (list (slot-value obj 'form) " "))))
    (cond
      ((and (null tokens) (null mtokens))
       (if (equal " " (car (last response)))
	   (subseq response 0 (1- (length response)))
	   response))

      ((and garbage-end (< (token-id (car tokens)) garbage-end))
       (sentence-text-aux (cdr tokens) mtokens garbage-end response))
      ((and garbage-end (equal (token-id (car tokens)) garbage-end))
       (sentence-text-aux (cdr tokens) mtokens nil response))
      
      ((and mtokens (<= (mtoken-start (car mtokens)) (token-id (car tokens))))
       (sentence-text-aux tokens (cdr mtokens)
			  (mtoken-end (car mtokens))
			  (append response (forma (car mtokens)))))
      (t
       (sentence-text-aux (cdr tokens) mtokens garbage-end (append response (forma (car tokens))))))))


(defun sentence->text (sentence)
  (format nil "~{~a~}" (sentence-text-aux (sentence-tokens sentence) (sentence-mtokens sentence) nil nil)))


(defun sentence-valid? (sentence)
  (and (every (lambda (tk)
		(not (equal (slot-value tk 'id)
			    (slot-value tk 'head))))
	      (sentence-tokens sentence))
       (some  (lambda (tk)
		(and (equal 0 (slot-value tk 'head))
		     (equal "root" (slot-value tk 'deprel))))
	      (sentence-tokens sentence))
       (sentence-meta-value sentence "text")
       (sentence-meta-value sentence "sent_id")))


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

