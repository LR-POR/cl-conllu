(in-package :cl-conllu)


(defun diff-sentences (p-sentence g-sentence evaluation-function confusion-table diff-table)
  
  (loop for p-token in (sentence-tokens p-sentence)
        for g-token in (sentence-tokens g-sentence) do

       (let ((p-value (funcall evaluation-function p-token p-sentence))
	     (g-value (funcall evaluation-function g-token g-sentence)))
	 (confusion-table-add-pair g-value p-value confusion-table)
	 (report-diff p-value g-value p-token g-token p-sentence g-sentence diff-table))))


(defun get-token-parent (token sent)
  (if (= (token-head token) 0)
      nil
      (token-parent token sent)))


(defun format-sentence-text (sentence &optional highlighted-tokens)
  (let ((ids (mapcar #'token-id highlighted-tokens)))
    (custom-sentence->text
     sentence
     :ignore-mtokens t
     :special-format-test
     #'(lambda (token)
	 (find (token-id token)
		ids))
     :special-format-function
     #'(lambda (string)
	 (format nil "<b>~a</b>" string)))))  


(defun format-dependency (token sent)
  (if (= (token-head token) 0)
      (format nil "~a (~a)" (token-deprel token) (token-form token))
      (format nil "~a (~a , ~a)" (token-deprel token) (token-form token) (token-form (token-parent token sent)))))

(defun format-dependency-pair (p-token g-token p-sentence g-sentence)
  (format nil "~a ~a" (html-set-font-color (format-dependency g-token g-sentence) "blue")
	              (html-set-font-color (format-dependency p-token p-sentence) "red")))


(defun html-set-font-color (text color)
  (format nil "<font color=\"~a\"> ~a </font>" color text))


(defun html-set-bold (text)
  (format nil "<b>~a</b>" text))


(defun html-make-info-line (topic line)
  (format nil "<p>~a ~a</p>"
	  (html-set-bold (format nil "~a:" topic ))
	  line))


(defun format-log-message (p-value g-value p-token g-token p-sentence g-sentence diff-table)
  (format nil "~{~a ~}<br>" (list (html-make-info-line "Id" (sentence-id g-sentence))
    (html-make-info-line "Text" (format-sentence-text g-sentence (list g-token (get-token-parent g-token g-sentence) )))
    (html-make-info-line "Dep" (format-dependency-pair p-token g-token p-sentence g-sentence)))))


(defun format-header (p-value g-value)
  (format nil "<h3>(~a ~a)</h3>" (html-set-font-color g-value "blue") (html-set-font-color p-value "red")))


(defun report-diff (p-value g-value p-token g-token p-sentence g-sentence diff-table)
  (when (not (string= g-value p-value))
    (let ((header (format-header p-value g-value))
	  (formatted-log (format-log-message p-value g-value p-token g-token p-sentence g-sentence diff-table)))
      
      (if (not (gethash header diff-table))
	  (setf (gethash header diff-table) (list formatted-log))
	  (push formatted-log (cdr (gethash header diff-table)))))))


(defun confusion-table-add-column (confusion-table new-column)
  (print new-column)
  ; Add new column
  (setf (gethash new-column confusion-table) (make-hash-table :test 'equal))

  ; Adds the cell value for the new column to each row
  (loop for row being the hash-values of confusion-table do
       (setf (gethash new-column row) 0))

  ; Adds value for every cell of the new column
  (loop for key being the hash-keys of confusion-table do
       (setf (gethash key (gethash new-column confusion-table)) 0)))


(defun confusion-table-add-pair (g-value p-value confusion-table)

  ; TODO - Simplify duplicate code

  ;(print (alexandria:hash-table-keys confusion-table))

  (when (not (gethash p-value confusion-table))
    (confusion-table-add-column confusion-table p-value))

  (when (not (gethash g-value confusion-table))
    (confusion-table-add-column confusion-table g-value))

  ;(print (gethash p-value (gethash g-value confusion-table)))
  (incf  (gethash p-value (gethash g-value confusion-table))))


(defun report-confusion-table (path golden-files prediction-files evaluation-function)

  (print golden-files)
  (print prediction-files)
  (let ((confusion-table (make-hash-table :test 'equal))
	(diff-table (make-hash-table :test 'equal)))
    
    (with-open-file (report path
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      
      (loop for p-file in prediction-files
	    for g-file in golden-files do
	   (print p-file)
	   (print g-file)
	   
	   (assert (string= (file-namestring p-file) (file-namestring g-file)) (p-file g-file) "invalid match of filenames")
	   
       (loop for p-sentence in (read-file p-file)
	     for g-sentence in (read-file g-file) do
	    (diff-sentences p-sentence g-sentence evaluation-function confusion-table diff-table)))
      
      (write-charset report)
      (write-style-css report)
      (write-confusion-table confusion-table report)
      (write-diffs diff-table report))))


(defun confusion-table-access-cell-counter (row col table)
  "Returns the value of the confusion table defined by the row and col"
  (gethash row (gethash col table)))


(defun confusion-table-rows (table)
  " Returns the rows of the confusion table "
  (let ((rows (list (alexandria:flatten (append '("") (alexandria:hash-table-keys table)))))
	(columns (alexandria:hash-table-keys table)))
    
    (loop for line in columns do
	 (let ((row (list line)))
	   (loop for column in columns do
		(append-element (write-to-string (confusion-table-access-cell-counter line column table)) row))
	   (append-element row rows)))
    rows))


(defun write-charset (stream)
  (write-line "<meta charset=\"UTF-8\">" stream))


(defun write-style-css (stream)
  (write-line "<style>
table, th, td {border: 1px solid black;border-collapse: collapse;padding: 5px;}
th, td {text-align: center;}
tr:first-child {color:blue; font-weight: bold;}
td:first-child { color:red; font-weight: bold;}
p {margin:0px;}
html * {font-family: Helvetica;}
</style>" stream))


(defun write-diffs (diff-table stream)
  (print "printing diff table..")
  (loop for header being the hash-keys in diff-table do
       (write-line header stream)
       (loop for diff in (gethash header diff-table) do
	    (write-line diff stream))))


(defun write-confusion-table (confusion-table stream)
  (let ((width 10)
	(data (confusion-table-rows confusion-table)))
    (write-line (format nil "<table> ~{ <tr> ~{ <td> ~{~Vd~} </td> ~} </tr> ~% ~} </table>"
	  (mapcar #'(lambda (r) (mapcar #'(lambda (v) (list width v)) r)) data)) stream)))


(defun custom-sentence->text (sentence &key (ignore-mtokens nil) (special-format-test #'null special-format-test-supplied-p) (special-format-function #'identity special-format-function-supplied-p))
  (assert (or (and special-format-test-supplied-p
		   special-format-function-supplied-p)
	      (and (not special-format-test-supplied-p)
		   (not special-format-function-supplied-p)))
	  (special-format-test
	   special-format-function)
	  "If a special format is intended, then both
	  SPECIAL-FORMAT-TEST and SPECIAL-FORMAT-FUNCTION should be
	  specified!")
  (assert (functionp special-format-test))
  (assert (functionp special-format-function))
  (labels ((forma (obj lst)
	     (let ((obj-form
		    (if (funcall special-format-test obj)
			(funcall special-format-function (slot-value obj 'form))
			(slot-value obj 'form))))
	       (if (search "SpaceAfter=No" (slot-value obj 'misc))
		   (cons obj-form lst)
		   (cons " " (cons obj-form lst)))))
	   (aux (tokens mtokens ignore response)
	     (cond 
	       ((and (null tokens) (null mtokens))
		(if (equal " " (car response))
		    (reverse (cdr response))
		    (reverse response)))

	       ((and ignore (< (token-id (car tokens)) ignore))
		(aux (cdr tokens) mtokens ignore response))
	       ((and ignore (equal (token-id (car tokens)) ignore))
		(aux (cdr tokens) mtokens nil response))
      
	       ((and mtokens (<= (mtoken-start (car mtokens)) (token-id (car tokens))))
		(aux tokens (cdr mtokens)
				   (mtoken-end (car mtokens))
				   (forma (car mtokens) response)))
	       (t
		(aux (cdr tokens) mtokens ignore (forma (car tokens) response))))))
    (format nil "~{~a~}" (aux (sentence-tokens sentence)
			      (if ignore-mtokens
				  nil
				  (sentence-mtokens sentence))
			      nil nil))))
