
(in-package :cl-conllu)

(defun select-sentence (id sentences)
  (find id sentences :key #'sentence-id :test #'equal))


(defun write-selected-sentence (filename id output)
  "Returns a sentence whose 'sent_id' from the list of sentences in
   'filename'."
  (let* ((sentences (read-conllu filename))
	 (selected (select-sentence id sentences)))
    (if selected
	(write-conllu-to-stream (list selected) output))))


(defun adjust-conllu (input-filename output)
  (write-conllu-to-stream (mapcar #'adjust-sentence (read-conllu input-filename))
			  output))


(defun modify-conllu (original-file changes-file output &optional (add-news nil))
  "The original file contains a set of sentences. The modified file
   contains some sentences from original modified, this function
   replaces in original the sentences presented in modified file,
   matching them using the sentence ids. If the modified file contains
   sentence not in original, the flag 'add-new' , if true, says that
   these sentence must be added in the end of the original file."
  (let ((originals (read-conllu original-file))
	(changes   (read-conllu changes-file)))
    (labels ((apply-changes (changes originals news)
	       (if changes
		   (let ((new (car changes)))
		     (if (select-sentence (sentence-id new) originals)
			 (apply-changes (cdr changes)
					(substitute-if new
						       (lambda (b)
							 (equal (sentence-id new)
								(sentence-id b)))
						       originals)
					news)
			 (apply-changes (cdr changes) originals
					(cons new news))))
		   (if add-news
		       (append originals (reverse news))
		       originals))))
      (write-conllu-to-stream (apply-changes changes originals nil) output))))
