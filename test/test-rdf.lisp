;;;; Test RDF conversion functions

(ql:quickload :cl-conllu)
(ql:quickload :prove)
;; (setf prove:*enable-colors* t)

(defpackage my-test
  (:use :cl
	:prove
	:cl-conllu))

(in-package :my-test)

(plan 1)

(subtest "Testing metadata with spaces"
  ;; Ideally it should be checked whether every IRI follows RFC 3987:
  ;; https://www.ietf.org/rfc/rfc3987.txt
  ;; However, there is no ready library for this.
  (let* ((example-relative-path
          "./test-data/metadata-example.conllu")
         (sentences
          (read-conllu (truename example-relative-path)))
         (my-stream
          (make-string-output-stream)))
    (conllu.rdf:convert-to-rdf sentences :stream my-stream)
    (let ((iri-with-whitespace
           (cl-ppcre:scan-to-strings
            "<[^>]* [^>]*>"
            (get-output-stream-string my-stream))))
      (ok (not iri-with-whitespace)
          (format nil "Iri with whitespace: ~a"
                  iri-with-whitespace)))))

(finalize)
