
(in-package :conllu.draw)


(defun tree-sentence (sentence &key (stream *standard-output*) show-meta)
  (when show-meta
    (mapc (lambda (p)
            (format stream "~a = ~a~%" (car p) (cdr p)))
          (sentence-meta sentence)))
   (format stream "~{~a ~%~}~%" (make-tree sentence))
  (values))


(defun make-tree (sent)
  (let*  ((tokens     (sentence-tokens sent))
          (root       (root-leaf tokens))
          (len-tokens (length tokens))
          (lines      (make-list len-tokens :initial-element ""))
          (spaces     (make-list len-tokens :initial-element 0)))
    (make-root-lines root lines spaces)
    (make-branchs root (children root tokens) tokens len-tokens lines spaces)
    (cons "─┮" lines)))


(defun root-leaf (tokens)
  (let ((token (first tokens)))
    (if (equal (slot-value token 'CL-CONLLU::HEAD) 0)
        token
        (root-leaf (rest tokens)))))


(defun make-root-lines (root lines spaces)
  (let ((id (- (slot-value root 'CL-CONLLU::ID) 1)))
    (loop for i from 0 to id do (add lines spaces i " │"))
    (add lines spaces id " ╰")))


(defun add (lines spaces id text)
  (setf (nth id lines)  text)
  (setf (nth id spaces) (length text)))


(defun children (father tokens)
  (let ((id-father (slot-value father 'CL-CONLLU::ID)))
    (remove-if-not
     (lambda (x)
       (or (equal (slot-value x 'CL-CONLLU::HEAD) id-father)
           (equal x father)))
     tokens)))


(defun make-branchs (father children tokens len-tokens lines spaces)
  (if (null father)
      (values)
      (let* ((to-do '())
             (bot   (- (slot-value (car children)        'CL-CONLLU::ID) 1))
             (top   (- (slot-value (car (last children)) 'CL-CONLLU::ID) 1))
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


(defun draw-node (id lines spaces node)
  (add lines spaces id (concatenate 'string (nth id lines) "─╼ "))
  (draw-node-values id lines spaces node))


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
