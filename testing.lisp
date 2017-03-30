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
