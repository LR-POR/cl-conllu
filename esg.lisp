(in-package :cl-conllu)

(defun g (key list)
  (cadr (assoc key list :test #'equal)))

(defun num-token-id (tk)
  (parse-integer (token-id tk)))

(defun replace-spaces (str)
  (substitute #\_ #\space str))

(defconstant +sentlenmax+ 100 "The maximum number of tokens in a sentence.  Needs to be the same value as in esg.")

(defun reorder-list (list i j)
  "Move J after I in list LIST.  Example:
   (reorder-list '(1 2 3 4 5 103 104) 2 4) => '(1 2 3 103 4 104)"
  (let ((tmp (cons (nth j list) (nthcdr (1+ i) list))))
    (rplacd (nthcdr (1- j) list) (nthcdr (1+ j) list))
    (rplacd (nthcdr i list) tmp)
    list))

(defun ph (ph head sentence)
  (let* ((metadata (cadr ph))
         (features (g "f" metadata))
         (split-features (split-sequence:split-sequence #\space features))
         (slot (g "slot" metadata))
         (id (g "id" metadata))
         (hd (g "hd" (cddr ph)))
         (word (replace-spaces (g "w" hd)))
         (citation-form (replace-spaces (g "c" hd)))
         (sense (replace-spaces (g "s" hd)))
         (frame (g "a" hd)))

    (dolist (c (cdr ph))
      (when (equal "ph" (car c)) (ph c id sentence))
      (when (equal "hd" (car c))
        (push
         (make-instance 'token :id id :form word :lemma citation-form
                        :upostag (car split-features)
                        :feats features :head head :deprel slot
                        :misc (format nil "s:~a a:~a" sense frame))
         (sentence-tokens sentence)))))

  sentence)

(defun segment (seg)
  (let ((sentence (make-instance 'sentence))
        (metadata (cadr seg))
        (top (caddr seg)))
    (ph top 0 sentence)
    (setf (sentence-meta sentence) (pairlis '("text") `(,(g "text" metadata))))
    (setf (sentence-tokens sentence) (reverse (sentence-tokens sentence)))
    sentence))

(defun read-esg (infile)
  (with-open-file (in infile)
    (let ((sentences (mapcar #'segment (cddr (xmls:parse in)))))
      sentences)))

