;; File for tests using :prove package ( https://github.com/fukamachi/prove )
;;
(ql:quickload :cl-conllu)
(ql:quickload :prove)
(setf prove:*enable-colors* t)

(defpackage my-test
  (:use :cl
	:prove
	:cl-conllu))

(in-package :my-test)

(let ((sents (cl-conllu:read-conllu #P"test/test.conllu"))
      (nk (make-instance 'cl-conllu:token
			 :id 9 :form "yesterday" :lemma "yesterday" :upostag "ADV"
			 :head 4 :deprel "advmod")))
  (cl-conllu:write-conllu sents #P"test/test-equal.conllu")
  (cl-conllu:write-conllu (list (cl-conllu:insert-token (car sents) nk)) #P"test/test-ins.conllu"))

(let ((sents (cl-conllu:read-conllu #P"test/test.conllu")))
  (cl-conllu:write-conllu (list (cl-conllu:remove-token (car sents) 7))  #P"test/test-del.conllu"))

(plan 2)

(let ((wrong-sent (car (read-conllu "test/test-adjust.conllu")))
      (right-sent (car (read-conllu "test/test-adjust-output.conllu"))))
  (subtest "adjust-sentence"
    (is
     (adjust-sentence wrong-sent)
     right-sent
     :test #'sentence-equal)))

(let ((sentence-1 (car (read-conllu "test/test-adjust-output.conllu")))
      (sentence-2 (car (read-conllu "test/nonprojective-test.conllu"))))
  (subtest "Projective and nonprojective sentences"
	   (is nil
	       (non-projective? sentence-1))
	   (is '((2 5) (4 8))
	       (non-projective? sentence-2))))

(finalize)
