(ql:quickload :xmls :silent t)
(ql:quickload :split-sequence :silent t)
(ql:quickload :cl-conllu :silent t)

(in-package :cl-conllu)

(defun g (key list)
  (cadr (assoc key list :test #'equal)))

(defun ph (ph head sentence)
  (let* ((metadata (cadr ph))
         (features (g "f" metadata))
         (split-features (split-sequence:split-sequence #\space features))
         (slot (g "slot" metadata))
         (id (g "id" metadata))
         (children (remove-if-not (lambda (x) (equal (car x) "ph")) (cdr ph)))
         (hd (g "hd" (cddr ph)))
         (word (g "w" hd))
         (citation-form (g "c" hd))
         (sense (g "s" hd))
         (frame (g "a" hd)))
    (push
     (make-instance 'token :id id :form word :lemma citation-form :upostag (car split-features)
                    :feats features :head head :deprel slot :misc (format nil "s:~a a:~a" sense frame))
     (sentence-tokens sentence))
    (dolist (c children) (ph c id sentence)))
  sentence)

(defun segment (seg)
  (let ((sentence (make-instance 'sentence))
        (metadata (cadr seg))
        (top (caddr seg)))
    (ph top 0 sentence)))

(defun process-esg (infile outfile)
  (with-open-file (in infile)
    (let ((sentences (mapcar #'segment (cdr (xmls:parse in)))))
      (write-conllu sentences outfile))))

