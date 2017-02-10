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
			    
(defun modify-conllu (orig-file mod-file &optional (add-new t))
  (let ((original (read-conllu orig-file))
	(modif (read-conllu mod-file))
	(new-sents nil))
    (dolist (mod-sentence modif)
      (let ((orig-sentence
	     (find (sentence-meta-value mod-sentence "sent_id")
		   original
		   :key (lambda (sent)
			  (sentence-meta-value sent "sent_id"))
		   :test #'equal)))
	(if orig-sentence
	    (setf original
		  (substitute mod-sentence
			      orig-sentence
			      original))
	    (push mod-sentence new-sents))))
    (if add-new
	(append original (reverse new-sents))
	original)))
     
