(in-package :conllu-visualize)


(defun tree-sentence (sentence)
  (progn
    (mapcar #'meta-sentence (sentence-meta sentence))
    (make-tree sentence)))


(defun meta-sentence (meta-sent)
  (format t "~a = ~a ~%" (car meta-sent) (cdr meta-sent)))


(defun make-tree (sent)
  (let*  ((tks (sentence-tokens sent))
	  (len-tks (length tks))
	  (lines (make-list len-tks :initial-element ""))
	  (spaces (make-list len-tks :initial-element 0))
	  (root (root-leaf tks)))
    (format t "─┮ ~%")
    (make-root-lines tks root lines spaces)
    (make-branchs root (childrens root tks) tks len-tks lines spaces)
    (mapcar #'(lambda (line) (format t "~a ~%" line)) lines)))


(defun make-branchs (father childs tokens len-tokens lines spaces)
  (if (null father)
      (values)
      (let* ((to-do '())
	    (bot (- (slot-value (car childs) 'CL-CONLLU::ID) 1))
	    (top (- (slot-value (car (last childs)) 'CL-CONLLU::ID) 1))
	    (father-id (slot-value father 'CL-CONLLU::ID))
	     (max-s (maximum-between spaces bot top)))
	;TO-DO : usar uma recurção aqui para subistituir o loop
	(loop for i from bot to top for a from 0 to len-tokens do
	      (let ((node (nth i tokens)))
		(if (equal node father)
		    (draw-father i lines spaces (equal bot i) (equal top i) node max-s)
		    (if (member node childs)
			(progn
			  (draw-descendent i lines spaces (equal bot i) (equal top i) max-s)
			  (if (is-not-just-a-leaf (+ 1 i) tokens)
			      (push node to-do)
			      (draw-node i lines spaces node)))
			(draw-no-descendets i lines spaces max-s)))))
	(loop for i in to-do do
	      (make-branchs i (childrens i tokens) tokens len-tokens lines spaces)))))

(defun draw-node (id lines spaces node)
  (setf (nth id lines) (concatenate 'string (nth id lines) "─╼ "))
  (setf (nth id spaces) (+ (nth id spaces) 3))
  (draw-node-values id lines spaces node))


(defun maximum-between (list bot top)
    (reduce #'max (between list bot top)))


(defun between (list bot top)
  (labels ((between-aux (list top count aux)
	     (if (equal top count)
		 (append aux (list (car list)))
		 (between-aux (cdr list) top (+ 1 count) (append aux (list (car list)))))))
    (between-aux (member (nth bot list) list) top bot nil)))


(defun is-not-just-a-leaf (id tokens)
  (remove-if-not (lambda (x) (equal (slot-value x 'CL-CONLLU::HEAD) id)) tokens))

(defun draw-descendent (id lines spaces bot top n)
  (let ((alinhar (make-string (- n (nth id spaces)) :initial-element #\space)))
    (if bot
	      (progn
		(setf (nth id lines) (concatenate 'string (nth id lines) alinhar " ╭"))
		(setf (nth id spaces) (+ (nth id spaces) 2)))
	      (if top
		  (progn
		    (setf (nth id lines) (concatenate 'string (nth id lines) alinhar " ╰"))
		    (setf (nth id spaces)  (+ (nth id spaces) 2)))
		  (progn
		    (setf (nth id lines) (concatenate 'string (nth id lines) alinhar " ├"))
		    (setf (nth id spaces)  (+ (nth id spaces) 2)))))))


(defun draw-no-descendets (id lines spaces n)
  (setf (nth id lines) (concatenate 'string (nth id lines)
				    (make-string (- n (nth id spaces))
						 :initial-element #\space) " │"))
  (setf (nth id spaces) (+ (nth id spaces) 2)))


(defun draw-father (id lines spaces bot top node n)
  (let ((alinhar (make-string (- n (nth id spaces)) :initial-element #\-)))
   (setf (nth id lines) (concatenate 'string (nth id lines) alinhar))
   (setf (nth id spaces) (+ (nth id spaces) 2))
   (draw-father-lines id lines spaces bot top)
   (draw-node-values id lines spaces node)))


(defun draw-father-lines (id lines spaces bot top)
  (if bot
      (progn
	(setf (nth id lines) (concatenate 'string (nth id lines) "─┮ "))
	(setf (nth id spaces) (+ (nth id spaces) 2)))
      (if top
	  (progn
	    (setf (nth id lines) (concatenate 'string (nth id lines) "─┶ "))
	    (setf (nth id spaces)  (+ (nth id spaces) 2)))
	  (progn
	    (setf (nth id lines) (concatenate 'string (nth id lines) "─┾ "))
	    (setf (nth id spaces)  (+ (nth id spaces) 2))))))


(defun draw-node-values (id lines spaces node)
  (let ((infos (collect-slots node '(CL-CONLLU::FORM CL-CONLLU::DEPREL) "")))
    (setf (nth id lines) (concatenate 'string (nth id lines) infos))
    (setf (nth id spaces)  (+ (nth id spaces) (length infos)))
    (values)))


(defun collect-slots (token slots acum)
  (if (null (cdr slots))
      (concatenate 'string acum (slot-value token (car slots)))
      (collect-slots token (cdr slots) (concatenate 'string acum (slot-value token (car slots)) " "))))


(defun make-root-lines (tokens root lines spaces )
  (let ((id (- (slot-value root 'CL-CONLLU::ID ) 1)))
    (if (> id 0)
	(progn
	  (loop for i from 0 to id do (progn (setf (nth i lines) " │")
					     (setf (nth i spaces) 2)))
	  (setf (nth id lines) " ╰")
	  (setf (nth id spaces) 2)
	  (values))
	(progn
	  (setf (nth id lines) " ╰")
	  (setf (nth id spaces) 2)))))


(defun root-leaf (tokens)
  (car (remove-if-not (lambda (x) (equal (slot-value x 'CL-CONLLU::HEAD) 0)) tokens)))


(defun childrens (father tokens)
  (let ((id-father (slot-value father 'CL-CONLLU::ID)))
    (remove-if-not
     (lambda (x) (or (equal (slot-value x 'CL-CONLLU::HEAD) id-father) (equal x father)))
     tokens)))


