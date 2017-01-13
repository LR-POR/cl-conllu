
(in-package :cl-conllu)

;; CORTE E COSTURA =  a:[pos=VERB] b:[lemma= "co.*"] => b:[pos="PART"]

;; Nossas regras:
;;
;; (=> ((a (pos "VERB") (lemma "co.*"))
;;      (b (lemma "de.*")))
;;     ((b (pos "PART"))))


;; START

;; recursao sobre a lista de sentencas
(defun apply-rules (sentences rules)
  "...") 

;; esta funcao recebe o token de uma sentença e verifica se o lhs
;; (lado esquerdo da regra) faz match com o token. Além de t ou nil
;; tavez esta função precise retornar mais coisas. vamos precisar dos
;; `binds`?
(defun match-token (token rule)
  "...")

;; para cada senteca, interando sobre os tokens, tenta aplicar uma
;; regra apos a outra e apos aplicar todas as regras avanca o
;; token. Se recursive, no final dos tokens, recomeça até não haver
;; mudanças.
(defun apply-rules-in-sentence (a-sentence rules &key (recursive t))
  "...") 
