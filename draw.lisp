
(in-package :conllu.draw)

;;; Abbreviations
;; dad     -> parent id
;; child   -> children
;; adopted -> nodes between the child of a node
;; kids    -> child and adopted
;; branch  -> (list dad kids)
;; len     -> length


(defun tree-sentence-by-id (path id)
  (let* ((p   (subseq id 0 (position #\- id)))
	 (ps  (subseq p 2))
	 (pb  (subseq p 0 2))
	 (fl  (format nil "~4,'0d" (parse-integer ps)))
	 (fp  (merge-pathnames (make-pathname :name (format nil "~a~a" pb fl) :type "conllu") path))
	 (s   (car (remove-if-not (lambda (s) (equal id (sentence-id s))) 
				  (cl-conllu:read-conllu fp)))))
    (if s (conllu.draw:tree-sentence s) (error "Sentence ~a not found in ~a" id fp))))


(defun tree-sentence (sentence &key
				 (fields-or-function (list 'CL-CONLLU::FORM 'CL-CONLLU::UPOSTAG
							   'CL-CONLLU::DEPREL 'CL-CONLLU::ID 'CL-CONLLU::HEAD))
				 (stream *standard-output*)
				 (show-meta nil))
  (when show-meta
    (mapc (lambda (p)
            (format stream "~a = ~a~%" (car p) (cdr p)))
          (sentence-meta sentence)))
  (let* ((tokens   (sentence-tokens sentence))
         (branches (list (cons 0 (get-kids 0 tokens t))))
         (lines    (make-tree fields-or-function tokens (make-list (1+ (length tokens)) :initial-element "") branches)))
    (format stream "~{~a ~%~}~%" lines)
    (values)))


(defun get-kids (dad tokens &optional stock? child adopted infant)
  (if tokens
      (let* ((token  (first tokens))
             (tokens (rest tokens))
             (id     (token-id token)))
        (cond ((equal dad id)
               (get-kids dad tokens t child infant infant))
              ((equal dad (token-head token))
               (get-kids dad tokens t (cons id child) infant infant))
              (t
               (get-kids dad tokens stock? child adopted (if stock? (cons id infant) nil)))))
      (cons (reverse child) (reverse adopted))))


(defun make-tree (fields-or-function tokens lines branches &optional branches-alt)
  (cond (branches
         (let* ((branch   (first branches))
                (branches (rest branches))
                (kids     (rest branch))
                (adopted  (rest kids)))
           (if (adopted-unfinished? lines adopted)
               (make-tree fields-or-function tokens lines branches (cons branch branches-alt))
               (let* ((dad   (first branch))
                      (child (first kids))
                      (data  (get-data fields-or-function tokens dad)))
                 (if child
                     (let ((lines        (make-twigs data dad child adopted lines))
                           (new-branches (mapcar #'(lambda (id) (cons id (get-kids id tokens)))
                                                 child)))
                       (make-tree fields-or-function tokens lines
                                  (append new-branches branches-alt branches)))
                     (let ((lines (update-lines dad (concatenate 'string "─╼" data) lines)))
                       (make-tree fields-or-function tokens lines
                                  (append branches-alt branches))))))))
        (branches-alt
         (let* ((branch   (first branches-alt))
                (branches (rest branches-alt))
                (kids     (rest branch))
                (adopted  (rest kids))
                (dad      (first branch))
                (child    (first kids))
                (data     (get-data fields-or-function tokens dad)))
           (if child
               (let ((lines        (make-twigs data dad child adopted lines))
                     (new-branches (mapcar #'(lambda (id) (cons id (get-kids id tokens)))
                                           child)))
                 (make-tree fields-or-function tokens lines (append new-branches branches)))
               (let ((lines (update-lines dad (concatenate 'string "─╼" data) lines)))
                 (make-tree fields-or-function tokens lines branches-alt)))))
        (t lines)))


(defun adopted-unfinished? (lines ids)
  (when ids
    (let* ((line (nth (first ids) lines))
           (len  (1- (length line))))
      (if (or (equal len -1) (equal (elt line len) #\│) (equal (elt line len) #\space))
          (adopted-unfinished? lines (rest ids))
          t))))


(defun get-data (fields-or-function tokens id)
  (if (listp fields-or-function)
      (if (equal id 0) " "
	  (reduce #'(lambda (data field)
		      (format nil " ~a~a" (slot-value (nth (1- id) tokens) field) data))
		  (reverse fields-or-function) :initial-value " "))
      (if (equal id 0) " "
	  (funcall fields-or-function (nth (1- id) tokens)))))


(defun make-twigs (data dad child adopted lines)
  (let* ((stroke-size (get-stroke-size adopted lines))
         (space-size  (max stroke-size (length (nth dad lines))))
         (lines       (reduce #'(lambda (lines id) (update-lines id " │" lines space-size #\space))
                              adopted :initial-value lines)))
    (cond ((> dad (apply #'max child))
           (let* ((lines (update-lines dad (concatenate 'string "─┶" data) lines stroke-size #\─))
                  (lines (update-lines (first child) " ╭" lines space-size #\space)))
             (reduce #'(lambda (lines id) (update-lines id " ├" lines space-size #\space))
                     (rest child) :initial-value lines)))
          ((< dad (apply #'min child))
           (let* ((lines (update-lines dad (concatenate 'string "─┮" data) lines stroke-size #\─))
                  (child (reverse child))
                  (lines (update-lines (first child) " ╰" lines space-size #\space)))
             (reduce #'(lambda (lines id) (update-lines id " ├" lines space-size #\space))
                     (rest child) :initial-value lines)))
          (t
           (let* ((lines (update-lines dad (concatenate 'string "─┾" data) lines stroke-size #\─))
                  (lines (update-lines (first child) " ╭" lines space-size #\space))
                  (lines (update-lines (first (last child)) " ╰" lines space-size #\space))
                  (len   (length child)))
             (reduce #'(lambda (lines id) (update-lines id " ├" lines space-size #\space))
                     (if (> len 2) (subseq child 1 (1- len)) nil) :initial-value lines))))))


(defun get-stroke-size (adopted lines)
  (if adopted
      (reduce #'(lambda (size id) (max size (length (nth id lines))))
              (rest adopted) :initial-value (length (nth (first adopted) lines)))
      0))


(defun update-lines (id text lines &optional size char)
  (let* ((old-line   (nth id lines))
         (complement (if size
                         (make-string (max 0 (- size (length old-line))) :initial-element char)
                         "")))
    (append (subseq lines 0 id)
            (list (concatenate 'string old-line complement text))
            (subseq lines (1+ id)))))
