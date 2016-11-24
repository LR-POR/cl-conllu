
(in-package :cl-conllu)

;; alternatives
;; http://stackoverflow.com/questions/3672880/more-generic-lisp-code-to-generate-combinations-of-pairs
;; http://stackoverflow.com/questions/28504467/combining-list-of-list
;; https://groups.google.com/forum/#!topic/comp.lang.lisp/NrMe2NmcCLU
;; https://goo.gl/KjclLT

(defun distance (s1 s2 &key test)
  (let* ((width (1+ (length s1)))
	 (height (1+ (length s2)))
	 (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
	(setf (aref d (1+ y) (1+ x))
	      (min (1+ (aref d y (1+ x)))
		   (1+ (aref d (1+ y) x))
		   (+ (aref d y x)
		      (if (funcall test (aref s1 x) (aref s2 y))
			  0
			  1))))))
    (aref d (1- height) (1- width))))


(defun diff-aux (sentences-1 sentences-2 distances limit test key)
  (if sentences-1
      (let ((a (car sentences-1)))
	(diff-aux (cdr sentences-1) sentences-2
		  (cons (cons a (loop with size = (sentence-size a)
				      for b in sentences-2
				      for diff = (funcall test (funcall key a) (funcall key b))
				      when (< (/ diff size) limit)
				      collect (list b diff)))
			distances)
		  limit test key))
      (reverse distances)))

(defun diff (sentences-a sentences-b &key limit test key)
  (diff-aux sentences-a sentences-b nil limit test key))


(defun print-diff (matriz)
  (mapcar (lambda (line)
	    (cons (sentence-meta (car line))
		  (mapcar (lambda (pair)
			    (list (sentence-meta (car pair))
				  (cadr pair)))
			  (cdr line))))
	  matriz))


(defun find-min (list &key (min nil))
  (cond
    ((null list)
     min)
    ((or (null min)
	 (< (caddr (car list))
	    (caddr min)))
     (find-min (cdr list) :min (car list)))
    (t (find-min (cdr list) :min min))))




