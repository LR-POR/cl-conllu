
(in-package :cl-conllu)

;; alternatives
;; http://stackoverflow.com/questions/3672880/more-generic-lisp-code-to-generate-combinations-of-pairs
;; http://stackoverflow.com/questions/28504467/combining-list-of-list
;; https://groups.google.com/forum/#!topic/comp.lang.lisp/NrMe2NmcCLU
;; https://goo.gl/KjclLT

(defun levenshtein (s1 s2 &key test)
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


(defun diff (sentences-a sentences-b &key test key)
  (let* ((dim (list (length sentences-a)
		    (length sentences-b)))
	 (distances (make-array dim :initial-element -1)))
    (dotimes (line (car dim) distances)
      (format t "Computing line [~a]~%" line)
      (dotimes (column (cadr dim))
	(setf (aref distances line column)
	      (funcall test
		       (funcall key (nth line sentences-a))
		       (funcall key (nth column sentences-b))))))))


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




(defun insert-at (lst index newelt)
  (if (equal index 0)
      (cons newelt lst)
      (progn
	(push newelt (cdr (nthcdr (1- index) lst)))
	lst)))
