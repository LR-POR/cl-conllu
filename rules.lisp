
(in-package :cl-conllu)


;; CORTE E COSTURA =  a:[pos=VERB] b:[lemma= "co.*"] => b:[pos="PART"]

;; Nossas regras:
;;
;; (=> ((a (pos "VERB") (lemma "co.*"))
;;      (b (lemma "de.*")))
;;     ((b (pos "PART"))))


;; START

;; recursao sobre a lista de sentencas
;(defun apply-rules (sentences rules)
 ; (dolist (sentece sentences) (apply-rules-in-sentence sentece)))

;; esta funcao recebe o token de uma sentença e verifica se o lhs
;; (lado esquerdo da regra) faz match com o token. Além de t ou nil
;; tavez esta função precise retornar mais coisas. vamos precisar dos
;; `binds`?

;; rule ::= (=> rls rhs)
;;rls ::= pattern+ rhs ::= pattern+ pattern := (var conditions+) condition := (field expression)


(defun match-token (token pattern)
  (if (null pattern)
      t
      (let* ((condition (car pattern))
	     (field  (car condition))
	     (regex (cadr condition)))
	(if (cl-ppcre:scan-to-strings regex (slot-value token (intern (symbol-name field) :cl-conllu)))
	    (match-token token (cdr pattern))
	    nil)))) 

    
;; para cada senteca, interando sobre os tokens, tenta aplicar uma
;; regra apos a outra e apos aplicar todas as regras avanca o
;; token. Se recursive, no final dos tokens, recomeça até não haver
;; mudanças.
(defun corte-e-costura (conllu-file lisp-rule-file name-of-new-conllu)
  (let ((sentences (read-conllu conllu-file))
	(rules (read-rules lisp-rule-file)))
    (apply-rules-in-sentences sentences rules)
    (write-conllu sentences name-of-new-conllu)))

(defun apply-rules-in-sentences (sentences rules)
  (dolist (sentence sentences) (apply-rules-in-sentence sentence rules)))


(defun apply-rules-in-sentence (a-sentence rules)
  (if (null rules)
      a-sentence
      (apply-rules-in-sentence (apply-rule-in-sentence a-sentence (car rules))
			       (cdr rules))))


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
	    (regex (cadr condition)))
	(setf (slot-value token (intern (symbol-name field) :cl-conllu)) regex)
	(apply-conditions-in-token (cdr conditions) token))))


;; rules

(defun rls (rule)
  (cadr rule))

(defun rhs (rule)
  (caddr rule))

(defun read-rules (filename)
  (with-open-file (stream filename)
    (loop for i = (read stream nil) while i collect i)))


