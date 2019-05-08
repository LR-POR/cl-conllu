
(in-package :conllu.draw)

;;; Abbreviations
;; dad     -> parent id
;; child   -> children
;; adopted -> nodes between the child of a node
;; kids    -> child and adopted
;; branch  -> (list dad kids)
;; len     -> length

(defun tree-sentence (sentence &key (fields (list 'CL-CONLLU::FORM 'CL-CONLLU::DEPREL))
				 (stream *standard-output*) show-meta)
  (when show-meta
    (mapc (lambda (p)
            (format stream "~a = ~a~%" (car p) (cdr p)))
          (sentence-meta sentence)))
  (let* ((tokens   (sentence-tokens sentence))
         (branches (list (cons 0 (get-kids 0 tokens t))))
         (lines    (make-tree fields tokens (make-list (1+ (length tokens)) :initial-element "") branches)))
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


(defun make-tree (fields tokens lines branches)
  (if branches
      (let* ((branch   (first branches))
             (branches (rest branches))
             (kids     (rest branch))
             (adopted  (rest kids)))
        (if (adopted-unfinished? lines adopted)
            (make-tree fields tokens lines (append branches (list branch)))
            (let* ((dad   (first branch))
                   (child (first kids))
                   (data  (get-data fields tokens dad)))
              (if child
                  (let ((lines        (make-twigs data dad child adopted lines))
                        (new-branches (mapcar #'(lambda (id) (cons id (get-kids id tokens))) child)))
                    (make-tree fields tokens lines (append new-branches branches)))
                  (let ((lines (update-lines dad (concatenate 'string "─╼" data) lines)))
                    (make-tree fields tokens lines branches))))))
      lines))


(defun adopted-unfinished? (lines ids)
  (when ids
    (let* ((line (nth (first ids) lines))
           (len  (1- (length line))))
      (if (or (equal len -1) (equal (elt line len) #\│) (equal (elt line len) #\space))
          (adopted-unfinished? lines (rest ids))
          t))))


(defun get-data (fields tokens id)
  (if (equal id 0) " "
      (reduce #'(lambda (data field)
                  (concatenate 'string " " (slot-value (nth (1- id) tokens) field) data))
              (reverse fields) :initial-value " ")))


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
