
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

(defun insert-mtokens (sentence mtokens &key (if-exists 'do-nothing))
  (mapc (lambda (mtoken)
	  (let ((existing-mtoken
		 (find-if (lambda (x)
			    (and
			     (eq (mtoken-start x) (mtoken-start mtoken))
			     (eq (mtoken-end x) (mtoken-end mtoken))))
			  (sentence-mtokens sentence))))
	    (if existing-mtoken
		(case if-exists
		  (('do-nothing) nil)
		  (('overwrite)
		   (remove existing-mtoken (sentence-mtokens sentence))
		   (push mtoken (sentence-mtokens sentence)))
		  (('add)
		   (push mtoken (sentence-mtokens sentence))))
		(and
		 (push mtoken (sentence-mtokens sentence))))))
	mtokens)
  (setf (sentence-mtokens sentence) (sort (sentence-mtokens sentence) '< :key 'mtoken-start)))

(defun insert-mtoken (sentence mtoken &key (if-exists 'do-nothing))
  (insert-mtokens sentence (list mtoken) :if-exists if-exists))

(defun mtoken->tokens (sentence mtoken)
  (remove-if-not (lambda (x) (and (>= x (mtoken-start mtoken))
				  (<= x (mtoken-end mtoken))))
		 (sentence-tokens sentence)
		 :key 'token-id))

(defun push-token (sentence inserted-token)
  ;; Inserts token at sentence object.
  ;; Please not it won't be inserted exactly as given: it's ID will be the same (place where it'll be inserted) but it's head should point to id value prior to the insertion.
  ;;
  ;; For instance, consider the sentence "John eats cake". We'll have:
  ;; ID	FORM	HEAD
  ;; 1	John	2
  ;; 2	eats	0
  ;; 3	cake	2
  ;; 
  ;; We want to insert "chocolate", transforming it into:
  ;; ID	FORM	HEAD
  ;; 1	John	2
  ;; 2	eats	0
  ;; 3	chocolate	3
  ;; 4	cake	2
  ;; 
  ;; In this case, we'll insert a token object with values (:ID 3 :FORM chocolate :HEAD 3), as "cake" had id 3 before the insertion.
  (dolist (token (sentence-tokens sentence))
    (when (>= (token-id token)
	      (token-id inserted-token))
      (setf (slot-value token 'id)
	    (1+ (slot-value token 'id))))
    (when (>= (token-head token)
	      (token-id inserted-token))
      (setf (slot-value token 'head)
	    (1+ (slot-value token 'head)))))
  (labels ((if-exists (slot-name)
	     (if (slot-boundp inserted-token slot-name)
		 (slot-value inserted-token slot-name)
		 "_")))
    (push (make-instance 'token
			 :id (token-id inserted-token)
			 :form (if-exists 'form)
			 :lemma (if-exists 'lemma)
			 :upostag (if-exists 'upostag)
			 :xpostag (if-exists 'xpostag)
			 :feats (if-exists 'feats)
			 :head (if (>= (token-head inserted-token)
				       (token-id inserted-token))
				   (1+ (token-head inserted-token))
				   (token-head inserted-token))
			 :deprel (if-exists 'deprel)
			 :deps (if-exists 'deps)
			 :misc (if-exists 'misc))
	  (sentence-tokens sentence)))
  (setf (slot-value sentence 'tokens)
	(sort (sentence-tokens sentence) #'< :key #'token-id))
  sentence)
;; This can be improved defining and using an inserted-push that
;; inserts an element in a sorted list instead of inserting and then sorting everything

(defun pop-token (sentence id)
  ;; Output: removed token with id equal to `id`
  ;; Side-effects: token with id `id` is removed from sentence's token list
  (let ((removed-token (find id (sentence-tokens sentence) :key #'token-id)))
    (setf (slot-value sentence 'tokens)
	  (remove removed-token (sentence-tokens sentence)))
    (dolist (token (sentence-tokens sentence))
      (when (> (token-id token) id)
	(setf (slot-value token 'id)
	      (1- (slot-value token 'id))))
      (cond 
	((> (token-head token) id)
	 (setf (slot-value token 'head)
	       (1- (slot-value token 'head))))
	((= (token-head token) id)
	 ;; If a token's head was the removed token, it's new HEAD value is now "_"
	 (setf (slot-value token 'head)
	       "_")))) 
    removed-token))
