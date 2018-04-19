;;;; Confusion matrices
;;;; class definition, initiatlization, formatting, auxiliary functions, etc
;;;;
;;;; For two sets of analyses of a set of sentences, a confusion matrix compares them with respect to a label for each token. Each token of a sentence is classified by a pair of labels (L1, L2), meaning that the first analyses for the token labels it as L1, while the second analyses labels it as L2.

;;; TODOs
;; todo: change token class definition to include pointers to sentence
;; todo: change token initiation in order to include these pointers)

;;; class definition

(in-package :cl-conllu)

(defclass confusion-matrix ()
  ((corpus-id :initarg :corpus-id
	      :accessor :cm-corpus-id
	      :documentation "Identifier of the corpus or experiment.")
   (key-fn :initarg :key-fn
	   :initform #'token-upostag
	   :accessor :cm-key-fn
	   :documentation "Function used to label each token.")
   (test-fn :initarg :test-fn
	    :initform #'equal
	    :accessor :cm-test-fn
	    :documentation "Function which compares two labels. Typically a form of equality.")
   (columns :accessor :cm-columns
	    :documentation "Parameter which contains the contents of the confusion matrix."
	    ;; Hash table which maps to rows, which maps to a pair
	    ;; (count . list), where the list is a list of
	    ;; (sentence-id . token-id)
	    )))

(defmethod print-object ... ...)
(defmethod initialize-instance :after
    ((obj confusion-matrix) &key test-fn &allow-other-keys)
  (setf (cm-columns obj)
	(make-hash-table :test test-fn)))

;;; column creation

;;; initialization

(defun make-confusion-matrix (sents-1 sents-2
			      &key key-fn test-fn)
  ...)

(defun update-confusion-matrix (sents-1 sents-2 cm)
  ...)

;;; Utility functions

(defun get-labels cn
  ;; output: list of labels
  ...)
(defun get-cell-count (label1 label2 cn)
  ;; output: int
  ...)
(defun get-cell-tokens (label1 label2 cn)
  ;; output: list of (sent-id . token-id)
  ...)

(defun get-sentences-ids (cn)
  ;; output: list of strings
  ...)
(defun get-exact-match-sentences (cn)
  ;; output: list of strings (sent-id)
  ...)
