;; (uncomment below to use this as a script)
;; #!/usr/local/bin/sbcl --script
;; (load "~/quicklisp/setup.lisp")
;; (ql:quickload :cl-conllu)


;; Usage:
;;
;; For isolating sentences from a .conllu file: ...
;;
;; For modifying a .conllu:
;; if X.conllu is the original file and Y.conllu is a new file,
;; outputs a Z.conllu, which is a modified X file, where, for each
;; sentence S:
;; - if S is in X and Y (checked via sent_id metavalue), use Y's version
;; - if S is only in X, keep it as it is
;; - if S is only in Y, depends on a "add-new" key
;;   - if its value is "y" or "t", then add at the end of the file
;;   - if it's value is "n" or "nil", then it doesn't appear in Z.
;; ...

;; Global variable for arguments when calling as a script with sbcl:
;; sb-ext:*posix-argv*

(in-package :cl-conllu)

(defun write-selected-sentence (filename sent-id)
  "If sent-id is a number, then returns a sentence whose sent_id is 'filename-sent-id'. If not, searches as a string exactly as given.
For instance, both calls below return the same:
  (write-selected-sentence \"CF1.conlllu\" 1)
  (write-selected-sentence \"CF1.conllu\" \"CF1-1\")"
  (let ((sentences (read-conllu filename))
	(desired-sentence nil))
    (if (numberp sent-id)
	(setf desired-sentence
	      (find  (concatenate
		      'string
		      (pathname-name filename) "-" (format nil "~a" sent-id))
		     sentences
		     :key (lambda (sent)
			    (sentence-meta-value sent "sent_id"))
		     :test #'equal))
	(setf desired-sentence
	      (find sent-id
		    sentences
		    :key (lambda (sent)
			   (sentence-meta-value sent "sent_id"))
		    :test #'equal)))
    (if desired-sentence
	(write-conllu-to-stream (cons desired-sentence nil) t))))
			    
