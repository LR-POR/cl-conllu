
(in-package :cl-conllu)

(let ((sents (cl-conllu:read-conllu #P"test/test.conllu"))
      (nk (make-instance 'cl-conllu:token
			 :id 9 :form "yesterday" :lemma "yesterday" :upostag "ADV"
			 :head 4 :deprel "advmod")))
  (cl-conllu:write-conllu sents #P"test/test-equal.conllu")
  (cl-conllu:write-conllu (list (cl-conllu:insert-token (car sents) nk)) #P"test/test-ins.conllu"))

(let ((sents (cl-conllu:read-conllu #P"test/test.conllu")))
  (cl-conllu:write-conllu (list (cl-conllu:remove-token (car sents) 7))  #P"test/test-del.conllu"))
