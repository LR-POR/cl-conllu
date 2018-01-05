(ql:quickload :cl-conllu)
(ql:quickload :alexandria)
(ql:quickload :cl-csv)

; Functions for generating and writing a confusion table in csv format
; For the purposes of documentation, the term 'pair' is a cons and 'pairs' a a-list
; The first element of the pair must be the prediction the second the golden i.e. (predicted-value . actual-value )

(defun get-columns (pairs)
  "Recieves a list of pairs, returns the unique values
   The values of the pairs must be strings
   e.g: (('1' . '2') ('1' . '3')) -> ('1' '2' 3)"
  (let ((columns nil))
	(loop for pair in pairs
	   when (not (find (car pair) columns :test #'string-equal )) do (push (car pair) columns)
	   when (not (find (cdr pair) columns :test #'string-equal )) do (push (cdr pair) columns))
	columns))

(defun make-row (columns)
  "Makes a confusion matrix row for the columns, initializes the values with 0
   Recieves the columns names
   i.e: row -> (col1 : 0 col2: 0)"
  (let ((row (make-hash-table :test 'equal)))
    (mapc (lambda(key)(setf (gethash key row) 0)) columns)
    row))

(defun make-empty-confusion-table (columns)
  "Returns an empty confusion table:
   i.e: table ->
   ((col1 : (col1 : 0 col2: 0 ...)
    (col2 : (col1 : 0 col2: 0 ...) ...)"
  (let ((table (make-hash-table :test 'equal))
	(row (make-row columns)))
    (mapc (lambda(key)(setf (gethash key table) (alexandria:copy-hash-table row))) columns)
    table))

(defun increment-cell-value (pair table)
  "Icrements the cell value of the confusion table defined by the pair"
  (incf (gethash (cdr pair) (gethash (car pair) table))))	

(defun make-confusion-table(pairs)
  "Returns a filled confusion table, recieves a-list of pairs"
  (let((table (make-empty-confusion-table (get-columns pairs))))
    (mapc (lambda (pair)(increment-cell-value pair table)) pairs)
    table))

(defun cf-access-cell (row col table)
  "Returns the value of the confusion table defined by the row and col"
  (gethash row (gethash col table)))

(defun append-element(element lst)
  "append element to end of the list"
  (if (= (length lst) 0)
      (push  element lst)
      (push element (cdr (last lst)))))

(defun confusion-table-rows (table)
  " Returns the rows for the csv file "
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

(defun save-as-csv (filename confusion-table)
  (make-csv filename (confusion-table-rows confusion-table)))

; Comparators

; Father head token comparator
(defun get-head-token(token sent)
  "Returns the token in the sentence that governs the sent token"
  (nth (- (cl-conllu:token-head token) 0)(cl-conllu:sentence-tokens sent)))

(defun same-head-tag(token sent g-token g-sent)
  "Recieves a list of tokens and it's sentence and the golden version of each
   Returns the pair:  predicted and actual father's UPOSTAG
   i.e: ('NOUN' . 'VERB')"
  (let ((father (get-head-token token sent))
	(g-father (get-head-token g-token g-sent)))
	(cons (cl-conllu:token-upostag father) (cl-conllu:token-upostag g-father))))

(defun father-head-comparator(sent g-sent)
  "Checks if the parent heads are equal to each token of the sentence, except the root "
  ;Gets the tokens of the sentences removing the ones with no parent (head=0) 
  (let ((tokens (remove 0 (cl-conllu:sentence-tokens sent) :key #'cl-conllu:token-head))
	(g-tokens (remove 0 (cl-conllu:sentence-tokens g-sent) :key #'cl-conllu:token-head)))
    ;(print tokens)
    
    (loop for token in tokens for g-token in g-tokens
       collect (same-head-tag token sent g-token g-sent))))

; Dependency distace comparator
(defun father-distance(token sent g-token g-sent)
  (let ((id (cl-conllu:token-id token))
	(g-id (cl-conllu:token-id g-token))
	(head (cl-conllu:token-head token))
	(g-head (cl-conllu:token-head g-token)))
    (cons
     (write-to-string (- head id))
     (write-to-string (- g-head g-id)))))

(defun dependency-distance(sent g-sent)
    (let ((pairs nil))
    (loop
       for token in  (cl-conllu:sentence-tokens sent)
       for g-token in  (cl-conllu:sentence-tokens g-sent)
	 collect (father-distance token sent g-token g-sent))))

; DEPREL comparator
(defun get-deprels(deprel)
  (let ((deprels (list deprel) )
	(splitted (split-sequence:SPLIT-SEQUENCE #\: deprel)))
    (if (> (length splitted) 1)
	(push (nth 0 splitted) deprels))
    deprels))

(defun same-deprel-tag(sent g-sent)
  (let ((pairs nil))
    (loop
       for token in  (cl-conllu:sentence-tokens sent)
       for g-token in  (cl-conllu:sentence-tokens g-sent)	 
       do (loop for deprel in (get-deprels (cl-conllu:token-deprel token))
	     do (loop for g-deprel in (get-deprels (cl-conllu:token-deprel g-token))
		   do (push (cons deprel  g-deprel) pairs))))
  pairs))

;Compare by file & corpus

(defun get-files-pairs (files g-files sentence-comparator)
  (let ((pairs nil))
  (loop for file in files
        for g-file in g-files
        do (mapc (lambda(item)(push item pairs)) (get-file-pairs file g-file sentence-comparator)))
  pairs))

(defun get-file-pairs (file g-file sentence-comparator)
  (let ((pairs nil))
  (loop for sentence in (cl-conllu:read-file file)
        for g-sentence in (cl-conllu:read-file g-file)
        do (mapc (lambda(item)(push item pairs)) (funcall sentence-comparator sentence g-sentence)))
  pairs))

(defun tests ()
  (setf arquivo1 #P"./golden.conllu")
  (setf arquivo2 #P"./predicted.conllu")

  (setf predicted (cl-conllu:read-file arquivo1))
  (setf golden (cl-conllu:read-file arquivo2))

  (setf sent (nth 0 predicted))
  (setf g-sent (nth 0 golden))  
  ;; run comparator, get pairs, make confusion table, build file
  ;; single sentences

  ;;(setf pairs-sentence (comparator sent g-sent))
  ;;(setf pairs-sentence (same-deprel-tag sent g-sent))
  (setf pairs-sentence (dependency-distance sent g-sent))
  (print "making confusion table")
  ;(print pairs-sentence)
  
  ;; many sentences
  ;; TODO - list of files
  ;;(setf pairs-file (list-comparator predicted golden))
  
  ;; Get confusion table - single sentence
  (setf confusion-table (make-confusion-table pairs-sentence))
  ;(print (alexandria:hash-table-keys confusion-table))
  ;(print (confusion-table-rows confusion-table))
  (make-csv  #P"./teste.csv" (confusion-table-rows confusion-table))

  (print (get-file-pairs arquivo1 arquivo2 #'same-deprel-tag))
  (setf a (get-files-pairs (list arquivo1 arquivo2) (list arquivo2 arquivo1) #'same-deprel-tag))
  (setf a (get-files-pairs (list arquivo1 arquivo2) (list arquivo2 arquivo1) #'dependency-distance))
  (make-csv  #P"./teste.csv" (confusion-table-rows confusion-table ))
  )




