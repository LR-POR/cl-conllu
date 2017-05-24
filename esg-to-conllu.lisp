(ql:quickload "cl-conllu")
(ql:quickload "xmls")
(ql:quickload "cl-ppcre")

(in-package :cl-conllu)

;; ESGSentence -> CoNLL-USentence
;; produce a approximate CoNLL-U sentence from a ESG one

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun read-esg (esg-filename)
  (xmls:parse (file-string esg-filename)))

(defun get-attribute (attr xml)
  (xmls:xmlrep-attrib-value attr xml))

(defun node-name-equal? (xml name)
  (equal name (xmls:xmlrep-tag xml)))

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
  (let* ((form (xmls:xmlrep-attrib-value "w" xml))
	 (lemma (xmls:xmlrep-attrib-value "c" xml))
	 (s (xmls:xmlrep-attrib-value "s" xml))
	 (a (xmls:xmlrep-attrib-value "a" xml))
	 (deps (concatenate 'string "s:" (write-to-string s) " " "a:" (write-to-string a))))
    (setf (token-id token) id)
    (setf (token-form token) form)
    (setf (token-lemma token) lemma)
    (setf (token-deps token) deps)))

(defun process-token (sentence xml tokens token-id)
  (cond ((node-name-equal? xml "ph") (setf tokens (build-token-from-ph tokens xml)))
	((node-name-equal? xml "hd") (progn
				       (modify-token-from-hd xml (car tokens) token-id)
				       (insert-token sentence (car tokens))
				       (incf token-id)
				       (setf tokens (cdr tokens)))))
  (print tokens)
  (values token-id tokens))

(defun process-esg (sentence xml tokens token-id)
  (multiple-value-bind (token-id tokens) (process-token sentence xml tokens token-id))
  (if (car (xmls:xmlrep-children xml)) (process-esg sentence (car (xmls:xmlrep-children xml)) tokens token-id))
  (if (cdr (xmls:xmlrep-children xml)) (process-esg sentence (cdr (xmls:xmlrep-children xml)) tokens token-id))
  sentence)

(defun esg-to-conllu (esg-document)
  (let ((sentence (make-instance 'sentence))
	(tokens nil)
	(token-id 1)
	(xml esg-document))
    (set-text sentence xml)
    (process-esg sentence (car (xmls:xmlrep-children xml)) tokens token-id)))
