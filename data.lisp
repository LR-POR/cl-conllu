
(in-package :cl-conllu)

(defparameter *token-fields*
  '(id form lemma upostag xpostag feats head deprel deps misc))

(defclass abstract-token ()
  ((sentence :accessor token-sentence)
   (lineno   :initarg  :lineno
	     :accessor token-lineno)
   (form     :initarg  :form
	     :accessor token-form)
   (cfrom    :initarg  :cfrom
	     :initform -1
	     :accessor token-cfrom)
   (cto      :initarg  :cto
	     :initform -1
	     :accessor token-cto)
   (misc     :initarg  :misc
	     :initform "_"
	     :accessor token-misc)))

(defclass token (abstract-token)
  ((id      :initarg :id
	    :accessor token-id)
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
	    :accessor token-deps)))


(defclass etoken (abstract-token)
  ((prev    :initarg :prev
	    :accessor etoken-prev)
   (index   :initarg :index
	    :accessor etoken-index)
   (lemma   :initarg :lemma
	    :accessor etoken-lemma)
   (upostag :initarg :upostag
	    :initform "_"
	    :accessor etoken-upostag)
   (xpostag :initarg :xpostag
	    :initform "_"
	    :accessor etoken-xpostag)
   (feats   :initarg :feats
	    :initform "_"
	    :accessor etoken-feats)
   (deps    :initarg :deps
	    :initform "_"
	    :accessor etoken-deps)))


(defclass mtoken (abstract-token)
  ((start   :initarg :start
	    :accessor mtoken-start)
   (end     :initarg :end
	    :accessor mtoken-end)))


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
	    :accessor sentence-mtokens)
   (etokens :initarg :etokens
	    :initform nil
	    :accessor sentence-etokens)))


(defmethod print-object ((obj sentence) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a at ~a"
	    (sentence-meta-value obj "sent_id") 
	    (slot-value obj 'start))))

(defmethod print-object ((obj token) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s ~a #~a-~a-~a"
	    (slot-value obj 'form) (slot-value obj 'upostag)
	    (slot-value obj 'id) (slot-value obj 'deprel) (slot-value obj 'head))))

 
(defmethod initialize-instance :after ((obj sentence)
				       &key tokens &allow-other-keys)
  "Sets the TOKEN-SENTENCE slot for each token attributed to the initialize sentence OBJ."
  (mapc (lambda (tk) (setf (token-sentence tk) obj))
	tokens)
  (sentence-fill-offsets obj))


(defun sentence-fill-offsets (sentence)
  (with-slots (tokens mtokens) sentence
    (loop with cpos = 0
	  with next = 0
	  for ltk = nil then tk
	  for tk in (sort (copy-seq (append tokens mtokens)) #'<= :key #'token-lineno)
	  for form  = (token-form tk)
	  for spc   = (if (search "SpaceAfter=No" (token-misc tk)) 0 1)
	  for len   = (length form)
	  do (typecase tk
	       (token  (if (> (token-id tk) next)
			   (setf (token-cfrom tk) cpos
				 (token-cto   tk) (+ len cpos)
				 cpos             (+ cpos len spc))
			   (setf (token-cfrom tk) (token-cfrom ltk)
				 (token-cto   tk) (token-cto ltk))))
	       (mtoken (setf (token-cfrom tk) cpos
			     (token-cto tk)   (+ len cpos)
			     cpos             (+ cpos len spc)
			     next             (mtoken-end tk))))
	  finally (return sentence))))


(defun sentence-matrix (sentence)
  (let ((arr (make-array (list (length (sentence-tokens sentence)) 10)))
	(tb '(0 id 1 form 2 lemma 3 upostag 4 xpostag 5 feats 6 head 7 deprel 8 deps 9 misc)))
    (dolist (tk (sentence-tokens sentence) arr)
      (dotimes (v 10)
	(setf (aref arr (- (token-id tk) 1) v)
	      (if (member (getf tb v) '(id head))
		  (- (slot-value tk (getf tb v)) 1)
		  (slot-value tk (getf tb v))))))))


(defun sentence-root (sentence)
  (find-if (lambda (tk) (and (equal 0      (token-head tk))
			     (equal "root" (token-deprel tk))))
	   (sentence-tokens sentence)))


(defun sentence-binary-tree (sentence)
  "Based on the idea from [1], it produces a tree view of the
   sentence, still need to improve the priorities of children. 

   Code at https://github.com/sivareddyg/UDepLambda in file
   src/deplambda/parser/TreeTransformer.java method 'binarizeTree'

   [1] Siva Reddy, O. Tackstrom, M. Collins, T. Kwiatkowski, D. Das,
       M. Steedman, and M. Lapataw, Transforming Dependency Structures
       to Logical Forms for Semantic Parsing, Transactions of the
       Association for Computational Linguistics, pp. 127–140,
       Apr. 2016."
  (let* ((pri '("amod" "flat" "compound" "det" "cc"
		"appos" "acl" "cop" "dobj" "aux" "xcomp" "nmod"
		"advmod" "advcl" "nsubj" "case" "conj" "punct"))
	 (siz (length pri)))
    (labels ((priority (label default)
	       (or (position label pri) default))
	     (deep-aux (root)
	       (let ((children (sort (token-children root sentence)
				     (lambda (a b)
				       (< (priority (token-deprel a) (+ 1 siz))
					  (priority (token-deprel b) (+ 2 siz)))))))
		 (if (null children)
		     root
		     (let* ((c (car children))
			    (left (list (token-deprel c) root (deep-aux c))))
		       (dolist (child (cdr children) left)
			 (setf left
			       (list (token-deprel child) left (deep-aux child)))))))))
      (deep-aux (sentence-root sentence)))))


(defun sentence-hash-table (sentence)
  (let* ((tb      (alexandria:alist-hash-table (sentence-meta sentence) :test #'equal))
	 (fields '(id form lemma upostag xpostag feats head deprel deps misc)))
    (setf (gethash "tokens" tb)
	  (mapcar (lambda (tk)
		    (let ((dt (mapcar (lambda (field)
					(cons (symbol-name field) (slot-value tk field)))
				      fields)))
		      (alexandria:alist-hash-table dt :test #'equal))) 
		  (sentence-tokens sentence)))
    tb))

(defun sentence-meta-value (sentence meta-field)
    (cdr (assoc meta-field (sentence-meta sentence) :test #'equal)))


(defun sentence-id (sentence)
  (sentence-meta-value sentence "sent_id"))

(defun sentence-text (sentence)
  (sentence-meta-value sentence "text"))


(defun sentence->text (sentence &key (ignore-mtokens nil)
				  (special-format-test #'null special-format-test-supplied-p)
				  (special-format-function #'identity special-format-function-supplied-p))
  "Receives SENTENCE, a sentence object, and returns a string
   reconstructed from its tokens and mtokens.
   
   If IGNORE-MTOKENS, then tokens' forms are used. Else, tokens with
   id contained in a mtoken are not used, with mtoken's form being
   used instead.

   It is possible to special format some tokens. In order to do so,
   both SPECIAL-FORMAT-TEST and SPECIAL-FORMAT-FUNCTION should be
   passed. Then for each object (token or mtoken) for which
   SPECIAL-FORMAT-TEST returns a non-nil result, its form is modified
   by SPECIAL-FORMAT-FUNCTION in the final string."
   
  (assert (or (and special-format-test-supplied-p
		   special-format-function-supplied-p)
	      (and (not special-format-test-supplied-p)
		   (not special-format-function-supplied-p)))
	  (special-format-test
	   special-format-function)
	  "If a special format is intended, then both
	  SPECIAL-FORMAT-TEST and SPECIAL-FORMAT-FUNCTION should be
	  specified!")
  (assert (functionp special-format-test))
  (assert (functionp special-format-function))
  (labels ((forma (obj lst)
	     (let ((obj-form
		    (if (funcall special-format-test obj)
			(funcall special-format-function (slot-value obj 'form))
			(slot-value obj 'form))))
	       (if (search "SpaceAfter=No" (slot-value obj 'misc))
		   (cons obj-form lst)
		   (cons " " (cons obj-form lst)))))
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
    (format nil "~{~a~}" (aux (sentence-tokens sentence)
			      (if ignore-mtokens
				  nil
				  (sentence-mtokens sentence))
			      nil nil))))


(defun token-attached (tk sentence)
  (labels ((aux (tk q)
	     (cond
	       ((equal (token-head tk) 0) t)
	       ((member (token-id tk) q :test #'equal) nil)
	       (t (aux (token-parent tk sentence) (cons (token-id tk) q))))))
    (aux tk nil)))


(defun sentence-valid? (sentence)
  (and (every (lambda (tk)
		(and (not (equal (token-id tk)
				 (token-head tk)))
		     (token-attached tk sentence)))
	      (sentence-tokens sentence))
       (sentence-root sentence)
       (sentence-meta-value sentence "text")
       (equal (sentence-meta-value sentence "text")
	      (sentence->text sentence))
       (sentence-meta-value sentence "sent_id")))


(defun sentence-size (sentence)
  (length (sentence-tokens sentence)))


(defun token-children (token sentence &key (fn-filter nil))
  (let ((res (remove-if-not (lambda (tk)
			      (equal (token-head tk) (token-id token)))
			    (sentence-tokens sentence))))
    (if fn-filter (remove-if-not fn-filter res) res)))


(defun token-parent (token sentence)
  (nth (- (token-head token) 1)
       (sentence-tokens sentence)))


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
  (let ((head-1 (token-head (sentence-get-token-by-id sentence id-1))))
    (cond
      ((equal head-1 0) nil)
      ((equal head-1 id-2)
       (reverse (cons id-2 (cons id-1 alist))))
      (t
       (is-descendant? head-1 id-2 sentence (cons id-1 alist))))))
 

(defun adjust-sentence (sentence)
  "Receives a sentence and reenumerate IDs and HEAD values of each
   token so that their order (as in sentence-tokens) is respected."
  (with-slots (tokens mtokens) sentence
    (let* ((maps (cons `(0 . 0)
		       (mapcar (lambda (tk pos)
				 (cons (token-id tk) (1+ pos)))
			       tokens (alexandria:iota (length tokens)))))
	   (no-change  (every (lambda (p) (equal (car p) (cdr p))) maps)))
      (when (not no-change)
	(dolist (tk tokens)
	  (setf (token-id tk)   (cdr (assoc (token-id tk) maps))
		(token-head tk) (cdr (assoc (token-head tk) maps))))
	(dolist (mtk mtokens)
	  (setf (mtoken-start mtk) (cdr (assoc (mtoken-start mtk) maps))
		(mtoken-end mtk)   (cdr (assoc (mtoken-end mtk) maps)))))))
  sentence)


(defun simple-deprel (deprel)
  (car (ppcre:split ":" deprel)))


(defun token-equal (tk1 tk2 &key (fields *token-fields*) (test #'equal) (simple-dep nil))
  (every (lambda (field)
	   (if (and (equal field 'deprel) simple-dep)
	       (funcall test
			(simple-deprel (slot-value tk1 field))
			(simple-deprel (slot-value tk2 field)))
	       (funcall test
			(slot-value tk1 field)
			(slot-value tk2 field))))
	 fields))


(defun mtoken-equal (mtoken-1 mtoken-2)
  "Tests if, for each slot, mtoken-1 has the same values as mtoken-2."
  (every (lambda (slot)
	   (equal (slot-value mtoken-1 slot)
		  (slot-value mtoken-2 slot)))
	 '(start end form misc)))


(defun sentence-equal (sent-1 sent-2)
  "Tests if, for each slot, sent-1 has the same values as sent-2.
   For tokens and multiword tokens, it uses token-equal and
   mtoken-equal, respectively."
  (and (every
	(lambda (slot)
	  (equal (slot-value sent-1 slot)
		 (slot-value sent-2 slot)))
	'(start meta))
       (if (equal (length (sentence-tokens sent-1))
		  (length (sentence-tokens sent-2)))
	   (dotimes (x (length (sentence-tokens sent-1)) t)
	     (unless (token-equal
		      (nth x (sentence-tokens sent-1))
		      (nth x (sentence-tokens sent-2)))
	       (return nil))))
       (if (equal (length (sentence-mtokens sent-1))
		  (length (sentence-mtokens sent-2)))
	   (dotimes (x (length (sentence-mtokens sent-1)) t)
	     (unless (mtoken-equal
		      (nth x (sentence-mtokens sent-1))
		      (nth x (sentence-mtokens sent-2)))
	       (return nil))))))


(defun token-misc-value  (token misc-key)
  "Assume token's MISC is a list of key-value pairs, return value
   corresponding to key MISC-KEY."
  (let* ((misc-str (token-misc token))
         (misc-elems (split-sequence:split-sequence #\| misc-str))
         (misc-pairs (mapcar (alexandria:curry #'split-sequence:split-sequence #\=) misc-elems)))
    (mapc (lambda (pair) (when (not (cdr pair))
                           (format *error-output*
                                   "token-misc-value: MISC field not a key-value pair: ~a"
                                   (car pair))))
          misc-pairs)
    (second (assoc misc-key misc-pairs :test #'string=))))

(defun sentence-get-token-by-id (sentence id)
  (nth (1- id) (sentence-tokens sentence)))

(defun sentence-by-id (id filename) 
  (find-if (lambda (s) (equal id (sentence-id s))) (read-conllu filename)))


