;; File for tests using :prove package ( https://github.com/fukamachi/prove )
;;
(ql:quickload :cl-conllu)
(ql:quickload :prove)

(defpackage my-test
  (:use :cl
	:prove
	:cl-conllu))

(in-package :my-test)

(plan 1)

(let ((wrong-sent (car (read-conllu "test/test-adjust.conllu")))
      (right-sent (car (read-conllu "test/test-adjust-output.conllu"))))
  (is
   (adjust-sentence wrong-sent)
   right-sent
   :test #'sentence-equal))
  
(finalize)
