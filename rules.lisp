
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
(defun match-token (token rule ref)
  (if (null rule)
      t
      (let* ((atom-rule (car rule))
	     (attribute (car atom-rule))
	     (regex (cadr atom-rule)))
	(if (cl-ppcre:scan-to-strings regex (funcall (getf *list* attribute) token))
	    (match-token token (cdr rule) ref)
	    nil))))


(defparameter *list*
  '(id cl-conllu:token-id
    form cl-conllu:token-form
    lemma cl-conllu:token-lemma
    upostag cl-conllu:token-upostag
    xpostag cl-conllu:token-xpostag
    feats cl-conllu:token-feats
    head cl-conllu:token-head
    deprel cl-conllu:token-deprel
    deps cl-conllu:token-deps
    misc cl-conllu:token-misc))

    
;; para cada senteca, interando sobre os tokens, tenta aplicar uma
;; regra apos a outra e apos aplicar todas as regras avanca o
;; token. Se recursive, no final dos tokens, recomeça até não haver
;; mudanças.


(defun apply-rules-in-sentence (a-sentece rules)
  (if (null rules)
      a-sentence
      (apply-rules-in-sentence (apply-rule-in-sentence a-sentece (car rule))
			       (cdr rules))))


(defun apply-rule-in-sentence (a-sentence rule)
  (let ((tokens (cl-conllu:sentence-tokens a-sentence)))
    (apply-rule-in-tokens tokens rule)))


(defun apply-rule-in-tokens (tokens rule &optional (old-tokens nil))
  (if (null tokens)
      old-tokens
      (if (apply? tokens (antecedente rule))
	  (let ((new-tokens (apply-changes tokens (consequente rule))))
	    (apply-rule-in-tokens new-tokens rule (if (null old-tokens)
						      (car new-tokens)
						      (append old-tokens (car new-tokens)))))
	  (apply-rule-in-tokens (cdr tokens) rule (if (null old-tokens)
						      (car tokens)
						      (append old-tokens (car tokens)))))))


(defun apply? (tokens rule)
  (if (null rule)
      t
      (if (null tokens)
	  nil
	  (if (match-token (car tokens) (cdar rule) (caar rule))
	      (apply? (cdr tokens) (cdr rule))
	      nil))))

(defun apply-changes (tokens rule)
  "------")
	  

;; rules

(defun antecedente (rule)
  (cadr rule))

(defun consequente (rule)
  (caddr rule))

(defun read-rules (filename)
  (with-open-file (stream filename)
    (loop for i = (read stream nil) while i collect i)))

 (cl-conllu:apply? tokens '((a (CL-CONLLU::LEMMA "além") (CL-CONLLU::FORM "A.*"))
			     (b 
