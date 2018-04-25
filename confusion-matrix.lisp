;;;; Confusion matrices
;;;; class definition, initiatlization, formatting, auxiliary functions, etc
;;;;
;;;; For two sets of analyses of a set of sentences, a confusion
;;;; matrix compares them with respect to a label for each token. Each
;;;; token of a sentence is classified by a pair of labels (L1, L2),
;;;; meaning that the first analyses for the token labels it as L1,
;;;; while the second analyses labels it as L2.

;;; class definition

(in-package :cl-conllu)

(defclass confusion-matrix ()
  ((corpus-id :initarg :corpus-id
	      :accessor confusion-matrix-corpus-id
	      :documentation "Identifier of the corpus or experiment.")
   (key-fn :initarg :key-fn
	   :initform #'token-upostag
	   :accessor confusion-matrix-key-fn
	   :documentation "Function used to label each token.")
   (test-fn :initarg :test-fn
	    :initform #'equal
	    :accessor confusion-matrix-test-fn
	    :documentation "Function which compares two labels.
	    Typically a form of equality.")
   (sort-fn :initarg :sort-fn
	    :initform #'(lambda (x y)
			  (string<=
			   (format nil "~a" x)
			   (format nil "~a" y)))
	    :accessor confusion-matrix-sort-fn
	    :documentation "Function which sorts labels. By default,
	    converts labels to string and uses lexicographical order")
   (rows :accessor confusion-matrix-rows
	 :documentation "Parameter which contains the contents of the
	 confusion matrix."
	 ;; Hash table which maps to rows, which are themselves
	 ;; hash tables that maps to an array #(COUNT LIST), where
	 ;; LIST is a list of (sentence-id . token-id)
	 )))


(defmethod initialize-instance :after
    ((obj confusion-matrix) &key &allow-other-keys)
  (setf (confusion-matrix-rows obj)
	(make-hash-table :test (confusion-matrix-test-fn obj))))

(defmethod print-object ((obj confusion-matrix) out)
  (print-unreadable-object (obj out :type t)
    (format out "~%~{~a~%~}"
	    (mapcar
	     #'(lambda (label-pair)
		 `(,(first label-pair)
		    ,(second label-pair)
		    ,(confusion-matrix-cell-count
		      (first label-pair)
		      (second label-pair)
		      obj)))
	     (confusion-matrix-cells-labels obj)))))

;;; Utility functions

(defun confusion-matrix-labels (cm)
  "Returns the list of all labels in the confusion matrix CM."
  ;; output: list of labels
  (sort
  (remove-duplicates
   (append
    (alexandria:hash-table-keys
     (confusion-matrix-rows cm))
    (apply #'append
	   (mapcar
	    #'alexandria:hash-table-keys
	    (alexandria:hash-table-values (confusion-matrix-rows cm)))))
   :test (confusion-matrix-test-fn cm))
  (confusion-matrix-sort-fn cm)))

(defun confusion-matrix-cells-labels (cm)
  "Returns a list of '(LABEL1 LABEL2) for each cell in the confusion
matrix CM."
  ;; output: list of pairs of labels
  (apply #'append
  (mapcar
   #'(lambda (row)
       (mapcar
	#'(lambda (column)
	    `(,row ,column))
	(sort (alexandria:hash-table-keys (gethash
				     row
				     (confusion-matrix-rows cm)))
	      (confusion-matrix-sort-fn cm))))
   (sort (alexandria:hash-table-keys (confusion-matrix-rows cm))
	 (confusion-matrix-sort-fn cm)))))

(defun confusion-matrix-cell-count (label1 label2 cm)
  "Returns the number of tokens that are contained in the cell defined
by LABEL1 LABEL2 in the confusion matrix CM."
  ;; output: int
  (let ((entry-array
	 (gethash label2
		  (gethash label1 (confusion-matrix-rows cm)))))
    (if entry-array
	(aref
	 entry-array
	 0)
	(error "There is no cell (~a ~a)."
	       label1
	       label2))))

(defun confusion-matrix-cell-tokens (label1 label2 cm)
  "Returns the list of (SENT-ID . TOKEN-ID) of tokens in the cell
LABEL1 LABEL2."
  ;; output: list of (sent-id . token-id)
  (let ((entry-array
	 (gethash label2
		  (gethash label1 (confusion-matrix-rows cm)))))
    (if entry-array
	(aref
	 entry-array
	 1)
	(error "There is no cell (~a ~a)."
	       label1
	       label2))))

;; TODO
;; (defun confusion-matrix-sentences-ids (cm)
;;   ;; output: list of strings
;;   ...)
;; (defun confusion-matrix-exact-match-sentences (cm)
;;   ;; output: list of strings (sent-id)
;;   ...)

;;; initialization

(defun make-confusion-matrix (list-sent1 list-sent2
			      &key corpus-id (key-fn #'token-upostag) (test-fn #'equal)
				(sort-fn #'(lambda (x y)
					     (string<=
					      (format nil "~a" x)
					      (format nil "~a" y)))))
  "Creates a new confusion matrix from the lists of sentences
LIST-SENT1 and LIST-SENT2."
  (assert (equal
	   (length list-sent1)
	   (length list-sent2))
	  ()
	  "LIST-SENT1 and LIST-SENT2 should have the same number of sentences!")
  (let ((cm (make-instance 'confusion-matrix
			   :test-fn test-fn
			   :key-fn key-fn
			   :corpus-id corpus-id
			   :sort-fn sort-fn)))
    (confusion-matrix-update list-sent1 list-sent2 cm)))

(defun confusion-matrix-update (list-sent1 list-sent2 cm)
  "Updates an existing confusion matrix by a list of sentences
LIST-SENT1 and LIST-SENT2."
  (assert (equal
	   (length list-sent1)
	   (length list-sent2))
	  ()
	  "LIST-SENT1 and LIST-SENT2 should have the same number of sentences!")
  (mapc
   #'(lambda (sent1 sent2)
       (confusion-matrix-update-sentences sent1 sent2 cm))
   list-sent1
   list-sent2)
  cm)

;;; low-level updating

(defun confusion-matrix-update-sentences (sent1 sent2 cm)
  "Updates an existing confusion matrix by a pair of matching
sentences SENT1 and SENT2. That is, SENT1 and SENT2 should be
alternative analyses of the same natural language sentence."
  (assert (equal
	   (sentence-size sent1)
	   (sentence-size sent2))
	  ()
	  "SENTENCES~%~a~%and~%~a~% do not have the same number of tokens!"
	  sent1
	  sent2)
  (assert (equal
	   (sentence-id sent1)
	   (sentence-id sent2))
	  ()
	  "SENTENCES~%~a~%and~%~a~% do not have the same ID!"
	  sent1
	  sent2)
  (mapc
   #'(lambda (tk1 tk2)
       (confusion-matrix-update-tokens
	tk1 tk2 cm))
   (sentence-tokens sent1)
   (sentence-tokens sent2)))

(defun confusion-matrix-update-tokens (token1 token2 cm)
  (assert (equal
	   (token-id token1)
	   (token-id token2))
	  ()
	  "Different tokens are being compared! Tokens ~a and ~a do not have the same ID. ~%Perhaps different sentences are being compared."
	  token1 token2)
  (assert (equal
	   (token-form token1)
	   (token-form token2))
	  ()
	  "Different tokens are being compared! Tokens ~a and ~a do not have the same FORM. ~%Perhaps different sentences are being compared."
	  token1 token2)

  (insert-entry-confusion-matrix
   (funcall (confusion-matrix-key-fn cm)
	    token1)
   (funcall (confusion-matrix-key-fn cm)
	    token2)
   token1
   cm))

(defun insert-entry-confusion-matrix (label1 label2 token cm)
  "Inserts TOKEN as an occurence in the cell LABEL1 LABEL2 of the
confusion matrix CM."
  (unless (existing-cell-p label1 label2 cm)
    (create-cell label1 label2 cm))
  (let ((cell (gethash
	       label2
	       (gethash label1 (confusion-matrix-rows cm)))))
    (incf (aref cell 0))
    (push `(,(sentence-id (token-sentence token))
	     ,(token-id token))
	  (aref cell 1))
    cell))

(defun existing-cell-p (label1 label2 cm)
  "Predicate for verifying whether the cell for row LABEL1 and column
LABEL2 already exist in the confusion matrix CM."
  (when (member label1
		(alexandria:hash-table-keys
		 (confusion-matrix-rows cm))
		:test (confusion-matrix-test-fn cm))
    (let ((row (gethash label1
			(confusion-matrix-rows cm))))
      (assert (hash-table-p row))
      (if (member label2
		  (alexandria:hash-table-keys
		   row)
		  :test (confusion-matrix-test-fn cm))
	  t))))

(defun create-cell (label1 label2 cm)
  "Creates the cell for row LABEL1 and column LABEL2 in confusion
matrix CM."
  (unless (member label1
		(alexandria:hash-table-keys
		 (confusion-matrix-rows cm))
		:test (confusion-matrix-test-fn cm))
    (setf (gethash label1
		   (confusion-matrix-rows cm))
	  (make-hash-table :test
			   (confusion-matrix-test-fn cm))))
  (let ((row (gethash label1
		      (confusion-matrix-rows cm))))
    (unless (member label2
		    (alexandria:hash-table-keys
		     row)
		    :test (confusion-matrix-test-fn cm))
      (setf (gethash label2
		     row)
	    (make-array
	     2
	     :initial-contents '(0 ()) )))))

;;; content adjustment

(defun normalize-confusion-matrix (cm)
  "Creates empty cells for each pair (LABEL1 LABEL2) of labels in
 (confusion-matrix-labels CM)."
  (let ((cm-labels (confusion-matrix-labels cm)))
    (dolist (label1 cm-labels)
      (unless (member label1
                      (alexandria:hash-table-keys
                       (confusion-matrix-rows cm))
                      :test (confusion-matrix-test-fn cm))
        (setf (gethash label1
                       (confusion-matrix-rows cm))
              (make-hash-table :test
                               (confusion-matrix-test-fn cm))))
      (let ((row (gethash label1
			  (confusion-matrix-rows cm))))
	(dolist (label2 cm-labels)
          (unless (member label2
			  (alexandria:hash-table-keys
			   row)
			  :test (confusion-matrix-test-fn cm))
	    (setf (gethash label2
			   row)
		  (make-array
		   2
		   :initial-contents '(0 ()) ))))))))
