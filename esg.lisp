(in-package :cl-conllu)

(defun avalue (key list)
  (cadr (assoc key list :test #'equal)))

(defun num-token-id (tk)
  (parse-integer (token-id tk)))

(defun replace-spaces (str)
  (substitute #\_ #\space str))

(defun ph (ph head sentence)
  (let* ((metadata (cadr ph))
         (features (avalue "f" metadata))
         (split-features (split-sequence:split-sequence #\space features))
         (slot (avalue "slot" metadata))
         (id (avalue "id" metadata))
         (hd (avalue "hd" (cddr ph)))
         (word (replace-spaces (avalue "w" hd)))
         (citation-form (replace-spaces (avalue "c" hd)))
         (sense (replace-spaces (avalue "s" hd)))
         (frame (avalue "a" hd)))

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
    (setf (sentence-meta sentence) (pairlis '("text") `(,(avalue "text" metadata))))
    (setf (sentence-tokens sentence) (reverse (sentence-tokens sentence)))
    (adjust-sentence sentence)))

(defun read-esg (infile)
  (with-open-file (in infile)
    (let ((sentences (mapcar #'segment (cddr (xmls:parse in)))))
      sentences)))

