
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


(defun remove-mtoken (sentence start)
  (let ((mtoken-to-remove
	 (find-if (lambda (mtk)
		     (equal (mtoken-start mtk) start))
		  (sentence-mtokens sentence))))
    (cond (mtoken-to-remove
	   (setf (sentence-mtokens sentence)
		 (remove mtoken-to-remove
			 (sentence-mtokens sentence)))
	   sentence)
	  (t
	   (format t "WARNING: There's no multiword token starting with ~a to remove.~%~%" start)
	   sentence))))
  

(defun mtoken->tokens (sentence mtoken)
  (remove-if-not (lambda (x) (and (>= x (mtoken-start mtoken))
				  (<= x (mtoken-end mtoken))))
		 (sentence-tokens sentence)
		 :key 'token-id))

(defun push-token (sentence inserted-token)
  " Inserts token at sentence object.  Please not it won't be inserted
   exactly as given: it's ID will be the same (place where it'll be
   inserted) but it's head should point to id value prior to the
   insertion. "
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
    (insert-at (sentence-tokens sentence)
	       (1- (token-id inserted-token))
	       (make-instance 'token
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
			      :misc (if-exists 'misc)))))

(defun pop-token (sentence id)
  (let ((removed-token (find id (sentence-tokens sentence) :key #'token-id))
	(is-head (find-if
		  (lambda (x) (equal id (token-head x)))
		  (sentence-tokens sentence)))
	(in-mtoken
	 (dolist (interval
		   (mapcar
		    #'(lambda (mtk)
			(list (mtoken-start mtk)
			      (mtoken-end mtk)))
		    (sentence-mtokens sentence)))
	   (cond
	     ((> (car interval) id)
	      (return nil))
	     ((>= (cadr interval) id)
	      (return interval))))))
    (cond ((null removed-token)
	   (format t "WARNING: There's no token ~a.~%~%" id)
	   sentence)
	  (in-mtoken
	   (format t "WARNING: This token is contained in the multiword token ~{~a-~a~}.~%Not removing.~%Remove the multiword token before removing this token.~%~%" in-mtoken)
	   sentence)
	  (is-head
	   (format t "WARNING: This token is the head of token ~a.~%Not removing.~%Please change its head before removing this token.~%~%" (token-id is-head))
	   sentence)
	  (t
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
		(setf (slot-value token 'head)
		      "_"))))
	   removed-token))))

(defun set-head (sentence id new-head &optional deprel)
  (let ((token (find id (sentence-tokens sentence) :key #'token-id))
	(descendant?
	 (is-descendant? new-head id sentence)))
    (cond
      (descendant?
       (format t "A token cannot have a descendant as head.~%Token ~a is descendant of token ~a, via path ~{~a, ~}. Nothing changed.~%~% "
	       new-head id descendant?))
      ((equal id new-head)
       (format t "A token cannot have itself as head. Nothing changed.~%~%"))
      (t
       (setf (slot-value token 'head)
	     new-head)
       (if deprel
	   (setf (slot-value token 'deprel)
		 deprel))))))


(defun is-descendant? (id-1 id-2 sentence)
  (is-descendant?-aux id-1 id-2 sentence nil))


(defun is-descendant?-aux (id-1 id-2 sentence alist)
  (let ((father-of-1-id (token-head
			 (find id-1 (sentence-tokens sentence) :key #'token-id))))
    (cond
      ((eq father-of-1-id 0)
       nil)
      ((eq father-of-1-id id-2)
       (reverse (cons id-2 (cons id-1 alist))))
      (t
       (is-descendant?-aux father-of-1-id id-2 sentence (cons id-1 alist))))))

