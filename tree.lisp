(in-package :conllu-visualize)


(defun tree-sentence (sentence &optional (stream *standard-output*))
  (mapc (lambda (p)
	  (format stream "~a = ~a~%" (car p) (cdr p)))
	(sentence-meta sentence))
  (format stream "~{~a ~%~}~%" (make-tree sentence))
  (values))


(defun make-tree (sent)
  (let*  ((tks (sentence-tokens sent))
	  (len-tks (length tks))
	  (lines  (make-list len-tks :initial-element ""))
	  (spaces (make-list len-tks :initial-element 0))
	  (root (root-leaf tks)))
    (make-root-lines tks root lines spaces)
    (make-branchs root (children root tks) tks len-tks lines spaces)
    (cons "─┮" lines)))


(defun make-branchs (father children tokens len-tokens lines spaces)
  (if (null father)
      (values)
      (let* ((to-do '())
	     (bot (- (slot-value (car children) 'CL-CONLLU::ID) 1))
	     (top (- (slot-value (car (last children)) 'CL-CONLLU::ID) 1))
	     (father-id (slot-value father 'CL-CONLLU::ID))
	     (max-s (maximum-between spaces bot top)))
     ;TO-DO : usar uma recurção aqui para subistituir o loop
	(loop for index from bot to top do
	      (let ((node (nth index tokens)))
		(if (equal node father)
		    (draw-father index lines spaces (equal bot index) (equal top index) node max-s)
		    (if (member node children)
			(progn
			  (draw-descendent index lines spaces (equal bot index)
					   (equal top index) max-s)
			  (if (is-not-just-a-leaf (+ 1 index) tokens)
			      (push node to-do)
			      (draw-node index lines spaces node)))
			(draw-no-descendets index lines spaces max-s)))))
	(loop for i in to-do do
	      (make-branchs i (children i tokens) tokens len-tokens lines spaces)))))

(defun draw-node (id lines spaces node)
  (add lines spaces id (concatenate 'string (nth id lines) "─╼ "))
  (draw-node-values id lines spaces node))


(defun maximum-between (list bot top)
  (reduce #'max (between list bot top)))


(defun between (list bot top)
  (labels ((between-aux (list top count aux)
	     (if (equal top count)
		 (append aux (list (car list)))
		 (between-aux (cdr list) top (+ 1 count) (append aux (list (car list))))))
	   (starting (list bot count)
	     (if (equal bot count)
		 list
		 (starting (cdr list) bot (+ 1 count)))))
    (between-aux (starting list bot 0) top bot nil)))


(defun is-not-just-a-leaf (id tokens)
  (remove-if-not (lambda (x) (equal (slot-value x 'CL-CONLLU::HEAD) id)) tokens))


(defun draw-descendent (id lines spaces bot top n)
  (let ((alinhar (make-string (- n (nth id spaces)) :initial-element #\space)))
    (if bot
	(add lines spaces id (concatenate 'string (nth id lines) alinhar " ╭"))
	(if top
	    (add lines spaces id (concatenate 'string (nth id lines) alinhar " ╰"))
	    (add lines spaces id (concatenate 'string (nth id lines) alinhar " ├"))))))


(defun draw-no-descendets (id lines spaces n)
  (let ((alinhar (make-string (- n (nth id spaces)) :initial-element #\space)))
    (add lines spaces id (concatenate 'string (nth id lines) alinhar  " │"))))


(defun draw-father (id lines spaces bot top node n)
  (let ((alinhar (make-string (- n (nth id spaces)) :initial-element #\-)))
    (add lines spaces id (concatenate 'string (nth id lines) alinhar))
    (draw-father-lines id lines spaces bot top)
    (draw-node-values id lines spaces node)))


(defun draw-father-lines (id lines spaces bot top)
  (if bot
      (add lines spaces id (concatenate 'string (nth id lines) "─┮ "))
      (if top
	  (add lines spaces id (concatenate 'string (nth id lines) "─┶ "))
	  (add lines spaces id (concatenate 'string (nth id lines) "─┾ ")))))


(defun draw-node-values (id lines spaces node)
  (let ((infos (collect-slots node '(CL-CONLLU::FORM CL-CONLLU::DEPREL) "")))
    (add lines spaces id (concatenate 'string (nth id lines) infos))
    (values)))


(defun collect-slots (token slots acum)
  (if (null (cdr slots))
      (concatenate 'string acum (slot-value token (car slots)))
      (collect-slots token (cdr slots) (concatenate 'string acum (slot-value token (car slots)) " "))))


(defun make-root-lines (tokens root lines spaces )
  (let ((id (- (slot-value root 'CL-CONLLU::ID ) 1)))
    (if (> id 0)
	(progn
	  (loop for i from 0 to id do (add lines spaces i " │"))
	  (add lines spaces id " ╰")
	  (values))
	(progn
	  (add lines spaces id " ╰")))))


(defun root-leaf (tokens)
  (car (remove-if-not (lambda (x) (equal (slot-value x 'CL-CONLLU::HEAD) 0)) tokens)))


(defun children (father tokens)
  (let ((id-father (slot-value father 'CL-CONLLU::ID)))
    (remove-if-not
     (lambda (x) (or (equal (slot-value x 'CL-CONLLU::HEAD) id-father) (equal x father)))
     tokens)))


(defun add (lines spaces id text)
  (setf (nth id lines) text)
  (setf (nth id spaces) (length text)))

