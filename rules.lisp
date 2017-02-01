
(in-package :cl-conllu)


;; CORTE E COSTURA =  a:[pos=VERB] b:[lemma= "co.*"] => b:[pos="PART"]

;; Nossas regras:
;;
;; rule       ::= => rls rhs
;; rls, rhs   ::= (pattern+)
;; pattern    ::= (var condition+)
;; var        ::= ?.*
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
  (let ((rules (with-open-file (stream filename)
		 (loop for i = (read stream nil) while i collect i))))
    (mapcar #'intern-rule rules)))


;;formatacao e validacao de regra

(defun valid-vars (rule)
  (let ((vars (rls-vars (rls rule))))
    (if vars
	(rhs-vars vars (rhs rule))
	nil)))


(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))


(defun rls-vars (rls &optional (var-list nil))
  (if (null rls)
      var-list
      (let ((var (caar rls)))
	(if (or (and  (variable-p var) (not (member var var-list))) (eq '* var))
	    (rls-vars (cdr rls) (cons var var-list))
	    nil))))


(defun rhs-vars (rls-vars rhs)
  (if (null rhs)
      t
      (let ((var (caar rhs)))
	(if (and (variable-p var) (member var rls-vars))
	    (rhs-vars rls-vars (cdr rhs))))))


(defun intern-rule (rule)
  (let ((interned-rule (mapcar #'intern-sides rule)))
    (if (valid-vars interned-rule)
	interned-rule
	(warn "Há erro de formatacao na regra ~D" rule))))


(defun intern-sides (sides)
  (if (atom sides)
      sides
      (if (listp (car sides))
	  (mapcar #'intern-pattern sides)
	  (mapcar #'intern-pattern (list sides)))))


;; (defun intern-pattern (pattern)
;;   (if (atom pattern)
;;       (list pattern)
;;       (let ((var (car pattern))
;; 	    (conditions (cdr pattern)))
;; 	(append (list var) (mapcar
;; 		       #'(lambda (condition) (list (intern (symbol-name (car condition)) :cl-conllu)
;; 						   (cadr condition)))
;; 		       conditions)))))


(defun intern-pattern (pattern)
  (if (atom pattern)
      (list pattern)
      (mapcar
       #'(lambda (condition) (if (atom condition)
				 condition
				 (list (intern (symbol-name (car condition)) :cl-conllu)
					  (cadr condition))))
       pattern)))

;; 


(defun apply-rules (sentences rules)
  (dolist (sentence sentences)
    (dolist (rule rules) (apply-rule-in-sentence sentence rule))))


(defun apply-rule-in-sentence (a-sentence rule)
  (if (null rule)
      a-sentence
      (let ((tokens (cl-conllu:sentence-tokens a-sentence)))
	   (apply-rule-in-tokens tokens rule)
	   a-sentence)))


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
  (cond ((null rls) bindings)
	((null tokens) nil)
	((and (eq '* (caar rls)) (match-token (car tokens) (cdadr rls)))
	 (match? (cdr tokens) (cddr rls) (append bindings  (list (caadr rls) (car tokens)))))
	((eq '* (caar rls)) (match? (cdr tokens) rls bindings))
	((match-token (car tokens) (cdar rls))
	 (match? (cdr tokens) (cdr rls) (append bindings  (list (caar rls) (car tokens)))))))


(defun match-token (token pattern)
  (if (null pattern)
      t
      (let* ((condition (car pattern))
	     (field  (car condition))
	     (regex (cadr condition)))
	(if (cl-ppcre:scan regex (slot-value token field))
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
	(setf (slot-value token field) a-value)
	(apply-conditions-in-token (cdr conditions) token))))
