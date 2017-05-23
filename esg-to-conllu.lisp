(ql:quickload "cl-conllu")
(ql:quickload "xmls")
(ql:quickload "cl-ppcre")
(in-package :cl-conllu)

;; ESGSentence -> CoNLL-USentence
;; produce a approximate CoNLL-U sentence from a ESG one

(defun read-esg (esg-filename)
  (cxml:parse-file esg-filename (cxml-xmls:make-xmls-builder)))

(defun get-attribute (attr esg-document)
  (car (cdr (member attr (apply #'append (cxml-xmls:node-attrs esg-document)) :test #'equal))))

(defun node-name-equal? (xml name)
  (equal name (cxml-xmls:node-name xml)))

(defun set-text (sentence xml)
  (setf (sentence-meta sentence)
	(cons `("text" . ,(get-attribute "text" xml))
	      (sentence-meta sentence))))

(defun build-token-from-ph (tokens xml)
  (let* ((features (get-attribute "f" xml))
	 (slot (get-attribute "slot" xml))
	 (upostag (car (cl-ppcre:split "\\s" features)))
	 (feats (format nil "~{~a~^ ~}"
			(cdr (cl-ppcre:split "\\s" features)))))
    (cons (make-instance
	   'token :upostag upostag :feats feats :deprel slot :head 0)
	  tokens)))

(defun modify-token-from-hd (xml token id)
  (let* ((form (get-attribute "w" xml))
	 (lemma (get-attribute "c" xml))
	 (s (get-attribute "s" xml))
	 (a (get-attribute "a" xml))
	 (deps (concatenate 'string "s:" (write-to-string s) " " "a:" (write-to-string a))))
    (setf (token-id token) id)
    (setf (token-form token) form)
    (setf (token-lemma token) lemma)
    (setf (token-deps token) deps)))

(defun esg-to-conllu (esg-document)
  (let ((sentence (make-instance 'sentence))
	(tokens nil)
	(token-id 1))
    (do ((xml esg-document (xmls:xmlrep-children xml)))
	((equal xml nil) sentence)
      (setf tokens (get-tokens (car xml) token-id))
      (setf tokens (get-tokens (cdr xml)))
	
      (cond ((node-name-equal? xml "seg") (set-text sentence xml))
	    ((node-name-equal? xml "ph") (setf tokens (build-token-from-ph tokens xml)))
	    ((node-name-equal? xml "hd") (progn
					   (modify-token-from-hd xml (car tokens) token-id)
					   (insert-token sentence (car tokens))
					   (incf token-id)
					   (setf tokens (cdr tokens))))))))










