;;;; Test reader functions

;;; This is particularly important as at the time of writing of this
;;; file (2018.08.13), some CoNLL-U can not be read.

(ql:quickload :cl-conllu)
(ql:quickload :prove)
;; (setf prove:*enable-colors* t)

(defpackage my-test
  (:use :cl
	:prove
	:cl-conllu))

(in-package :my-test)

(plan 2)

(subtest "Testing '###' sentence from English EWT"
  (let* ((sentence-relative-path "./test-data/hashhashhash.conllu")
         (sentence
          (first (read-conllu sentence-relative-path))))
    (is-type sentence 'cl-conllu:sentence)))

(subtest "Testing empty notes from Enhanced Dependencies"
  (pass "Not yet implemented.")
  ;; TODO: implement
  )

(finalize)
