
(in-package :cl-conllu)

(defun sentence->text (sentence)
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (tk) (slot-value tk 'form))
		  (sentence-tokens sentence))))


(defun diff (sentences-a sentences-b &key test key)
  (labels ((diff-aux (sentences-1 sentences-2 distances test key)
	     (if sentences-1
		 (diff-aux (cdr sentences-1)
			   sentences-2
			   (cons (mapcar (lambda (current)
					   (list current
						 (car sentences-1)
						 (funcall test
							  (funcall key (car sentences-1))
							  (funcall key current))))
					 sentences-2)
				 distances)
			   test key)
		 (reverse distances))))
   (diff-aux sentences-a sentences-b nil test key)))


(defun find-min (list &key (min nil))
  (cond
    ((null list)
     min)
    ((or (null min)
	 (< (caddr (car list))
	    (caddr min)))
     (find-min (cdr list) :min (car list)))
    (t (find-min (cdr list) :min min))))
