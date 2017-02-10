
(in-package :cl-conllu)

(defclass token ()
  ((id      :initarg :id
	    :accessor token-id)
   (form    :initarg :form
	    :accessor token-form)
   (lemma   :initarg :lemma
	    :accessor token-lemma)
   (upostag :initarg :upostag
	    :initform "_"
	    :accessor token-upostag)
   (xpostag :initarg :xpostag
	    :initform "_"
	    :accessor token-xpostag)
   (feats   :initarg :feats
	    :initform "_"
	    :accessor token-feats)
   (head    :initarg :head
	    :initform "_"
	    :accessor token-head)
   (deprel  :initarg :deprel
	    :initform "_"
	    :accessor token-deprel)
   (deps    :initarg :deps
	    :initform "_"
	    :accessor token-deps)
   (misc    :initarg :misc
	    :initform "_"
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


(defun sentence->text (sentence)
  (labels ((forma (obj lst)
	     (if (search "SpaceAfter=No" (slot-value obj 'misc))
		 (cons (slot-value obj 'form) lst)
		 (cons " " (cons (slot-value obj 'form) lst))))
	   (aux (tokens mtokens ignore response)
	     (cond 
	       ((and (null tokens) (null mtokens))
		(if (equal " " (car response))
		    (reverse (cdr response))
		    (reverse response)))

	       ((and ignore (< (token-id (car tokens)) ignore))
		(aux (cdr tokens) mtokens ignore response))
	       ((and ignore (equal (token-id (car tokens)) ignore))
		(aux (cdr tokens) mtokens nil response))
      
	       ((and mtokens (<= (mtoken-start (car mtokens)) (token-id (car tokens))))
		(aux tokens (cdr mtokens)
				   (mtoken-end (car mtokens))
				   (forma (car mtokens) response)))
	       (t
		(aux (cdr tokens) mtokens ignore (forma (car tokens) response))))))
    (format nil "~{~a~}" (aux (sentence-tokens sentence) (sentence-mtokens sentence) nil nil))))


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
       (equal (sentence-meta-value sentence "text")
	      (sentence->text sentence))
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


(defun mtoken->tokens (sentence mtoken)
  (remove-if-not (lambda (x) (and (>= x (mtoken-start mtoken))
				  (<= x (mtoken-end mtoken))))
		 (sentence-tokens sentence)
		 :key 'token-id))


(defun insert-token (sentence new-token)
  "Inserts token in a sentence object. It will not be inserted exactly
   as given: it's ID will be the same (place where it'll be inserted)
   but it's head should point to id value prior to the insertion. It
   changes the sentence object passed."
  (with-slots (tokens) sentence
    (dolist (token tokens)
      (if (>= (token-id token) (token-id new-token))
	  (incf (token-id token)))
      (if (>= (token-head token) (token-id new-token))
	  (incf (token-head token))))
    (if (>= (token-head new-token)
	    (token-id new-token))
	(incf (token-head new-token)))
    (insert-at tokens (1- (token-id new-token)) new-token)
    sentence))


 


(defun remove-token (sentence id)
  "Remove the token with the given ID if it is not part of a
   multi-word token and it does not contain childs. It returns two
   values, the sentence (changed or not) and a boolean (nil if the
   sentence was not changed and true if changed. If the removed token
   is the root of the sentence, a new root must be provided."
  (with-slots (tokens mtokens) sentence
    (let ((to-remove (find id tokens :key #'token-id :test #'equal))
	  (childs (find id tokens :key #'token-head :test #'equal)))
      (cond ((some (lambda (mt) (<= (mtoken-start mt) id (mtoken-end mt)))
		   mtokens)
	     (values sentence nil))
	    ((or (null to-remove) childs)
	     (values sentence nil))
	    (t (dolist (token (sentence-tokens sentence))
		 (if (> (token-id token) id)
		     (decf (token-id token)))
		 (if (> (token-head token) id)
		     (decf (token-head token))))
	       (setf tokens (remove to-remove tokens))
	       (values sentence t))))))


(defun set-head (sentence id new-head &optional deprel)
  (let ((token (find id (sentence-tokens sentence)
		     :key #'token-id :test #'equal)))
    (cond
      ((is-descendant? new-head id sentence)
       (values sentence nil))
      ((equal id new-head)
       (values sentence nil))
      (t
       (setf (slot-value token 'head) new-head)
       (if deprel
	   (setf (token-deprel 'deprel) deprel))
       (values sentence t)))))


(defun is-descendant? (id-1 id-2 sentence &optional alist)
  (let ((parent (token-head (find id-1 (sentence-tokens sentence)
				  :key #'token-id :test #'equal))))
    (cond
      ((equal parent 0) nil)
      ((equal parent id-2)
       (reverse (cons id-2 (cons id-1 alist))))
      (t
       (is-descendant? parent id-2 sentence (cons id-1 alist))))))
 

(defun adjust-sentence (sentence)
  "Receives a sentence and reenumerate IDs and HEAD values of each
  token so that their order (as in sentence-tokens) is respected."
  (with-slots (tokens mtokens) sentence
    (let ((maps (cons `(0 . 0)
		      (mapcar (lambda (tk pos)
				(cons (token-id tk) (1+ pos)))
			      tokens (range (length tokens))))))
      (dolist (tk tokens)
	(setf (token-id tk)   (cdr (assoc (token-id tk) maps))
	      (token-head tk) (cdr (assoc (token-head tk) maps))))
      (dolist (mtk mtokens)
	(setf (mtoken-start mtk) (cdr (assoc (mtoken-start mtk) maps))
	      (mtoken-end mtk)   (cdr (assoc (mtoken-end mtk) maps))))))
  sentence)

