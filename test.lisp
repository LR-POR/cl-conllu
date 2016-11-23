
(in-package :cl-conllu)


(let ((sent (cl-conllu:read-conllu "pt-ud-train.conllu")))
  (write-conllu sent "teste.conllu"))

(let ((test (with-output-to-string (s)
	      (write-conllu-to-stream (cl-conllu:read-conllu "pt-ud-train.conllu") s))))
  (with-input-from-string (in test)
    (equal test (with-output-to-string (out)
		  (write-conllu-to-stream (read-conllu-from-stream in) out)))))
