(in-package :cl-conllu)


(defun make-pair-diff (sent g-sent token g-token evaluation-function)
  (let ((pair-diff (make-hash-table)))
    (setf (gethash "diffs" pair-diff) (cons (funcall evaluation-function token sent) (funcall evaluation-function g-token g-sent)))

    (when (not (string=
	      (car (gethash "diffs" pair-diff))
	      (cdr (gethash "diffs" pair-diff))))
    (setf (gethash "sent" pair-diff) sent)
    (setf (gethash "g-sent" pair-diff) g-sent)
    (setf (gethash "token" pair-diff) token)
    (setf (gethash "g-token" pair-diff) g-token))
    
    pair-diff))


(defun is-pair-different (pair)
    (if (not (string= (car pair) (cdr pair)))
	t))


(defun diff-sentences-by-function (sent g-sent evaluation-function)
  (loop
     for token in (sentence-tokens sent)
     for g-token in (sentence-tokens g-sent)
     collect (make-pair-diff sent g-sent token g-token evaluation-function)))


(defun parent-upostag (token sent)
  (if (= (token-head token) 0)
      "root"
      (token-upostag (token-parent token sent))))


(defun token-to-parent-distance(token sent)
  (if (= (token-head token) 0)
      (write-to-string 0)
      (write-to-string (- (token-head token) (token-id token)))))

(defun diff-parent-upostag (sent g-sent)
  "Checks if the parent heads are equal to each token of the sentence, except the root "
  (let ((diff (sentence-diff sent g-sent)))
    (loop
       for token in (sentence-tokens sent)
       for g-token in (sentence-tokens g-sent)
       collect (cons (parent-upostag token sent) (parent-upostag g-token g-sent)))))


(defun diff-dependency-distance(sent g-sent)
  (loop
     for token in (cl-conllu:sentence-tokens sent)
     for g-token in (cl-conllu:sentence-tokens g-sent)
     collect (cons (token-to-parent-distance token sent) (token-to-parent-distance g-token g-sent))))


(defun diff-deprel (sent g-sent)
  (let ((pairs nil))
    (loop
       for token in  (cl-conllu:sentence-tokens sent)
       for g-token in  (cl-conllu:sentence-tokens g-sent)
       collect (cons (token-deprel token) (token-deprel g-token)))))


; Functions for generating and writing a confusion table in csv format
; For the purposes of documentation, the term 'pair' is a cons and 'pairs' an a-list
; The first element of the pair must be the prediction the second the golden i.e. (predicted-value . actual-value)


(defun get-columns (pairs)
  "Recieves a list of pairs, returns the unique values
   The values of the pairs must be strings
   e.g: (('1' . '2') ('1' . '3')) -> ('1' '2' '3')"
  
  (let ((columns nil))
    (loop for pair in pairs
       when (not (find (format nil "~A" (car pair)) columns :test #'string-equal)) do (push (format nil "~A" (car pair)) columns)
       when (not (find (format nil "~A" (cdr pair)) columns :test #'string-equal)) do (push (format nil "~A" (cdr pair)) columns))
    columns))


(defun make-row (columns)
  "Makes a confusion matrix row for the columns, initializes the values with 0
   Recieves the columns names
   i.e: row -> (col1 : 0 col2: 0)"
  (let ((row (make-hash-table :test 'equal)))
    (mapc (lambda(key) (setf (gethash key row) 0)) columns)
    row))


(defun make-empty-confusion-table (columns)
  "Returns an empty confusion table:
   i.e: table ->
   ((col1 : (col1 : 0 col2: 0 ...)
    (col2 : (col1 : 0 col2: 0 ...) ...)"
  (let ((table (make-hash-table :test 'equal))
	(row (make-row columns)))
    (mapc (lambda(key) (setf (gethash key table) (alexandria:copy-hash-table row))) columns)
    table))


(defun increment-cell-value (pair table)
  "Icrements the cell value of the confusion table defined by the pair"
  (incf (gethash (format nil "~A" (cdr pair)) (gethash (format nil "~A" (car pair)) table))))


(defun make-confusion-table (diffs)
  "Returns a filled confusion table, recieves a-list of pairs"
  (let* ((pairs (mapcar #'(lambda (x) (gethash "diffs" x)) diffs))
         (table (make-empty-confusion-table (get-columns pairs))))

    (mapc (lambda (pair) (increment-cell-value pair table)) pairs)
    table))


(defun cf-access-cell (row col table)
  "Returns the value of the confusion table defined by the row and col"
  (gethash row (gethash col table)))


(defun append-element (element lst)
  "append element to end of the list"
  (if (= (length lst) 0)
      (push  element lst)
      (push element (cdr (last lst)))))


(defun confusion-table-rows (table)
  " Returns the rows of the confusion table "
  (let ((rows (list (alexandria:flatten (append '("") (alexandria:hash-table-keys table)))))
	(columns (alexandria:hash-table-keys table)))
    
    (loop for line in columns
       do
	 (let ((row (list line)))
	   (loop for column in columns
	      do
		(append-element (write-to-string (cf-access-cell line column table)) row))
	   (append-element row rows)))
    rows))


(defun make-csv (file lines)
  "Makes a csv file from the sent lines"
  (with-open-file (str file
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format str (cl-csv:write-csv lines))))


(defun save-confusion-table-csv (filename confusion-table)
  (make-csv filename (confusion-table-rows confusion-table)))


(defun report-write-confusion-table-string (confusion-table stream)
  (let ((width 10)(data (confusion-table-rows confusion-table)))
    
  (write-line (format nil "~{<tr>~{~{~Vd~}~}</tr>~%~}"
	  (mapcar #'(lambda (r) (mapcar #'(lambda (v) (list width v)) r)) data)) stream)))


(defun report-format-dependency (token sent)
  (if (= (token-head token) 0)
      (concatenate 'string (token-deprel token) "(" (token-form token) ")")
      (concatenate 'string (token-deprel token) "(" (token-form token) ";" (token-form (token-parent token sent)) ")")))
      
  
(defun report-format-dependency-diff (token g-token sent g-sent)
  (concatenate 'string (report-format-dependency token sent) " -- " (report-format-dependency g-token g-sent)))


(defun report-write-sentences(diffs stream)
    (let* ((pairs (mapcar #'(lambda (x) (gethash "diffs" x)) diffs))
	   (columns  (get-columns pairs)))

      (loop for col1 in columns do
	   (loop for col2 in columns do
		(write-line (concatenate 'string "(" col1 " - " col2 ")") stream)
		(write-line "" stream)

		(loop for diff in diffs do
		     (let ((pair (gethash "diffs" diff)))
		       (when (and (is-pair-different pair) (and (string= col1 (car pair)) (string= col2 (cdr pair))))
			 
			   (write-line (concatenate 'string (sentence-id (gethash "sent" diff))) stream)
			   (write-line (sentence-text (gethash "sent" diff)) stream)
			   (write-line (report-format-dependency-diff
					(gethash "token" diff)
					(gethash "g-token" diff)
					(gethash "sent" diff)
					(gethash "g-sent" diff)

					) stream)
			   
			   
			   			)))))))




(defun make-confusion-table-report (file diffs)

  (with-open-file (str file
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)

    (report-write-confusion-table-string (make-confusion-table diffs) str)
    (report-write-sentences diffs str)))
