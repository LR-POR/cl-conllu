#!/usr/bin/env /usr/local/bin/sbcl --script

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))

(ql:quickload '(:alexandria :cl-conllu) :silent t)

(in-package :cl-conllu)

(let ((cmd (nth 1 sb-ext:*posix-argv*)))
  (cond 
    ((equal cmd "select")
     (write-selected-sentence (nth 2 sb-ext:*posix-argv*)
			      (nth 3 sb-ext:*posix-argv*)
			      *standard-output*))
    ((equal cmd "modify")
     (modify-conllu (nth 2 sb-ext:*posix-argv*)
		    (nth 3 sb-ext:*posix-argv*)
		    *standard-output*))
    ((equal cmd "adjust")
     (adjust-conllu *standard-input*
		    *standard-output*))

    ((equal cmd "draw")
     (draw-conllu *standard-input*
		  *standard-output*))))

