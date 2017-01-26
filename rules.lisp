
(in-package :cl-conllu)


;; CORTE E COSTURA =  a:[pos=VERB] b:[lemma= "co.*"] => b:[pos="PART"]

;; Nossas regras:
;;
;; rule       ::= => rls rhs
;; rls, rhs   ::= (pattern+)
;; pattern    ::= (var condition+)
;; condition  ::= (field expression)
;; expression ::= string | regex
;;
;; (=> ((a (pos "VERB") (lemma "co.*"))
;;      (b (lemma "de.*")))
;;     ((b (pos "PART"))))




;; START

(defun corte-e-costura (conllu-file rules-file new-conllu-file)
  (let ((sentences (read-conllu conllu-file))
	(rules (read-rules rules-file)))
    (apply-rules sentences rules)
    (write-conllu sentences new-conllu-file)))


;;acessores e leitor de regras


(defun rls (rule)
  (cadr rule))


(defun rhs (rule)
  (caddr rule))


(defun read-rules (filename)
  (with-open-file (stream filename)
    (loop for i = (read stream nil) while i collect i)))


;; 


(defun apply-rules (sentences rules)
  (dolist (sentence sentences)
    (dolist (rule rules) (apply-rule-in-sentence sentence rule))))


(defun apply-rule-in-sentence (a-sentence rule)
  (let ((tokens (cl-conllu:sentence-tokens a-sentence)))
    (apply-rule-in-tokens tokens rule)
    a-sentence))


(defun apply-rule-in-tokens (tokens rule)
  (if (null tokens)
      (values)
      (progn
	(apply-changes tokens rule)
	(apply-rule-in-tokens (cdr tokens) rule))))


(defun apply-changes (tokens rule)
  (let  ((bindings (match? tokens (rls rule))))
    (if bindings
	(apply-rhs bindings (rhs rule))
	(values))))


(defun match? (tokens rls &optional (bindings nil))
  (if (null rls)
      bindings
      (if (null tokens)
	  nil
	  (if (match-token (car tokens) (cdar rls))
	      (match? (cdr tokens) (cdr rls) (append bindings  (list (caar rls) (car tokens))))
	      nil))))


(defun match-token (token pattern)
  (if (null pattern)
      t
      (let* ((condition (car pattern))
	     (field  (car condition))
	     (regex (cadr condition)))
	(if (cl-ppcre:scan regex (slot-value token (intern (symbol-name field) :cl-conllu)))
	    (match-token token (cdr pattern))
	    nil))))


(defun apply-rhs (bindings rhs)
  (if (null rhs)
      (values)
      (let* ((pattern (car rhs))
	    (var (car pattern))
	    (conditions (cdr pattern))
	    (token (getf bindings var)))
	(apply-conditions-in-token conditions token)
	(apply-rhs bindings (cdr rhs)))))


(defun apply-conditions-in-token (conditions token)
  (if (null conditions)
      (values)
      (let* ((condition (car conditions))
	    (field (car condition))
	    (a-value (cadr condition)))
	(setf (slot-value token (intern (symbol-name field) :cl-conllu)) a-value)
	(apply-conditions-in-token (cdr conditions) token))))
