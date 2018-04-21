;;;; Confusion matrices
;;;; class definition, initiatlization, formatting, auxiliary functions, etc
;;;;
;;;; For two sets of analyses of a set of sentences, a confusion matrix compares them with respect to a label for each token. Each token of a sentence is classified by a pair of labels (L1, L2), meaning that the first analyses for the token labels it as L1, while the second analyses labels it as L2.

;;; class definition

(in-package :cl-conllu)

(defclass confusion-matrix ()
  ((corpus-id :initarg :corpus-id
	      :accessor cm-corpus-id
	      :documentation "Identifier of the corpus or experiment.")
   (key-fn :initarg :key-fn
	   :initform #'token-upostag
	   :accessor cm-key-fn
	   :documentation "Function used to label each token.")
   (test-fn :initarg :test-fn
	    :initform #'equal
	    :accessor cm-test-fn
	    :documentation "Function which compares two labels. Typically a form of equality.")
   (rows :accessor cm-rows
	 :documentation "Parameter which contains the contents of the confusion matrix."
	 ;; Hash table which maps to rows, which are themselves
	 ;; hash tables that maps to an array #(COUNT LIST), where
	 ;; LIST is a list of (sentence-id . token-id)
	 )))


(defmethod print-object ((obj confusion-matrix) out)
  (print-unreadable-object (obj out :type t)
    (format out "~%~{~{~a~%~}~}"
	    (mapcar
	     #'(lambda (row)
		 (mapcar
		  #'(lambda (column)
		      `(,row
			,column
			,(gethash
			  column
			  (gethash row
				   (cm-rows obj)))))
		  (alexandria:hash-table-keys
		   (gethash row (cm-rows obj)))))
	     (alexandria:hash-table-keys (cm-rows obj))))))

(defmethod initialize-instance :after
    ((obj confusion-matrix) &key test-fn &allow-other-keys)
  (setf (cm-rows obj)
	(make-hash-table :test test-fn)))

;;; Utility functions

;; (defun get-labels cm
;;   ;; output: list of labels
;;   ...)
;; (defun get-cells-labels cm
;;   ;; output: list of pairs of labels
;;   ...)
;; (defun get-cell-count (label1 label2 cm)
;;   ;; output: int
;;   ...)
;; (defun get-cell-tokens (label1 label2 cm)
;;   ;; output: list of (sent-id . token-id)
;;   ...)
;; (defun get-sentences-ids (cm)
;;   ;; output: list of strings
;;   ...)
;; (defun get-exact-match-sentences (cm)
;;   ;; output: list of strings (sent-id)
;;   ...)

;;; initialization

(defun make-confusion-matrix (list-sent1 list-sent2
			      &key (key-fn #'token-upostag) (test-fn #'equal) corpus-id)
  (assert (equal
	   (length list-sent1)
	   (length list-sent2))
	  ()
	  "LIST-SENT1 and LIST-SENT2 should have the same number of sentences!")
  (let ((cm (make-instance 'confusion-matrix
			   :test-fn test-fn
			   :key-fn key-fn
			   :corpus-id corpus-id)))
    (update-confusion-matrix list-sent1 list-sent2 cm)))

(defun update-confusion-matrix (list-sent1 list-sent2 cm)
  (assert (equal
	   (length list-sent1)
	   (length list-sent2))
	  ()
	  "LIST-SENT1 and LIST-SENT2 should have the same number of sentences!")
  (mapc
   #'(lambda (sent1 sent2)
       (update-confusion-matrix-sentences sent1 sent2 cm))
   list-sent1
   list-sent2)
  cm)

;;; low-level updating

(defun update-confusion-matrix-sentences (sent1 sent2 cm)
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
       (update-confusion-matrix-tokens
	tk1 tk2 cm))
   (sentence-tokens sent1)
   (sentence-tokens sent2)))

(defun update-confusion-matrix-tokens (token1 token2 cm)
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
   (funcall (cm-key-fn cm)
	    token1)
   (funcall (cm-key-fn cm)
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
	       (gethash label1 (cm-rows cm)))))
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
		 (cm-rows cm))
		:test (cm-test-fn cm))
    (let ((row (gethash label1
			(cm-rows cm))))
      (assert (hash-table-p row))
      (if (member label2
		  (alexandria:hash-table-keys
		   row)
		  :test (cm-test-fn cm))
	  t))))

(defun create-cell (label1 label2 cm)
  "Creates the cell for row LABEL1 and column LABEL2 in confusion
matrix CM."
  (unless (member label1
		(alexandria:hash-table-keys
		 (cm-rows cm))
		:test (cm-test-fn cm))
    (setf (gethash label1
		   (cm-rows cm))
	  (make-hash-table :test
			   (cm-test-fn cm))))
  (let ((row (gethash label1
		      (cm-rows cm))))
    (unless (member label2
		    (alexandria:hash-table-keys
		     row)
		    :test (cm-test-fn cm))
      (setf (gethash label2
		     row)
	    (make-array
	     2
	     :initial-contents '(0 ()) )))))

;;; content (hash) adjustment

