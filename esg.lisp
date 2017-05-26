(ql:quickload :xmls :silent t)
(ql:quickload :split-sequence :silent t)
(ql:quickload :cl-conllu :silent t)

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

(defun reorder-punctuation (tokens)
  "See chapter 13 of Using Slot Grammar.  esg gives a special id for
certain punctuation tags.  It is always +sentlenmax+ plus the node
number of the preceeding word. If you are using a customized esg, you
need to update the +sentlenmax+ parameter."
  (mapc (lambda (x)
          (let* ((id (token-id x))
                 (previous (write-to-string (- (num-token-id x) +sentlenmax+)))
                 (pos-id (position id tokens :key #'token-id :test #'equal))
                 (pos-previous (position previous tokens :key #'token-id :test #'equal)))
            (when (> (num-token-id x) +sentlenmax+)
              (reorder-list tokens pos-previous pos-id)))) tokens))

(defun ph (ph head sentence)
  (let* ((metadata (cadr ph))
         (features (g "f" metadata))
         (split-features (split-sequence:split-sequence #\space features))
         (slot (g "slot" metadata))
         (id (g "id" metadata))
         (children (remove-if-not (lambda (x) (equal (car x) "ph")) (cdr ph)))
         (hd (g "hd" (cddr ph)))
         (word (replace-spaces (g "w" hd)))
         (citation-form (replace-spaces (g "c" hd)))
         (sense (replace-spaces (g "s" hd)))
         (frame (g "a" hd)))
    (push
     (make-instance 'token :id id :form word :lemma citation-form
                    :upostag (car split-features)
                    :feats features :head head :deprel slot
                    :misc (format nil "s:~a a:~a" sense frame))
     (sentence-tokens sentence))
    (dolist (c children) (ph c id sentence)))
  sentence)

(defun segment (seg)
  (let ((sentence (make-instance 'sentence))
        (metadata (cadr seg))
        (top (caddr seg)))
    (ph top 0 sentence)
    (setf (sentence-meta sentence) (pairlis '("text") `(,(g "text" metadata))))
    (setf (sentence-tokens sentence) (reorder-punctuation (sort (sentence-tokens sentence) #'< :key #'num-token-id)))
    sentence))

(defun process-esg (infile outfile)
  (with-open-file (in infile)
    (let ((sentences (mapcar #'segment (cddr (xmls:parse in)))))
      (write-conllu sentences outfile))))

