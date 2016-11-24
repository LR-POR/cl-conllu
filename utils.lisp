
(in-package :cl-conllu)

(defun sentence->text (sentence)
  (format nil "~{~a~^ ~}"
	  (mapcar (lambda (tk) (slot-value tk 'form))
		  (sentence-tokens sentence))))

;; alternatives
;; http://stackoverflow.com/questions/3672880/more-generic-lisp-code-to-generate-combinations-of-pairs
;; http://stackoverflow.com/questions/28504467/combining-list-of-list
;; https://groups.google.com/forum/#!topic/comp.lang.lisp/NrMe2NmcCLU
;; https://goo.gl/KjclLT

(defun diff (sentences-a sentences-b &key test key)
  (labels ((collect (a b)
	     (list a b
		   (funcall test (funcall key a) (funcall key b))))
	   (diff-aux (sentences-1 sentences-2 distances test key)
	     (if sentences-1
		 (diff-aux (cdr sentences-1)
			   sentences-2
			   (cons (mapcar (lambda (current) (collect (car sentences-1) current))
					 sentences-2)
				 distances)
			   test key)
		 (reverse distances))))
    (diff-aux sentences-a sentences-b nil test key)))



(defun diff-1 (sentences-a sentences-b &key test key)
  (loop for a in sentences-a
	collect (loop for b in sentences-b
		      collect (list a b (funcall test
						 (funcall key a)
						 (funcall key b))))))

(defun map-cartesian (fn bags)
  (labels ((gn (x y)
             (if y (mapcar (lambda (i) (gn (cons i x) (cdr y))) (car y))
                 (funcall fn x))))
    (gn nil (reverse bags))))

(defun diff-2 (sentences-a sentences-b &key test key)
  (map-cartesian (lambda (pair)
		   (destructuring-bind (a b)
		       pair
		     (list a b (funcall test
					(funcall key a)
					(funcall key b)))))
		 (list sentences-a sentences-b)))


(defun find-min (list &key (min nil))
  (cond
    ((null list)
     min)
    ((or (null min)
	 (< (caddr (car list))
	    (caddr min)))
     (find-min (cdr list) :min (car list)))
    (t (find-min (cdr list) :min min))))
