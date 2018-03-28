(ql:quickload :cl-conllu)
(ql:quickload :alexandria)


(defun parent-upostag (token sent)
  (if (= (cl-conllu:token-head token) 0)
      "root"
      (cl-conllu:token-upostag (cl-conllu::token-parent token sent))))


(defun token-to-parent-distance(token sent)
  (if (= (cl-conllu:token-head token) 0)
      (write-to-string 0)
      (write-to-string (- (cl-conllu:token-head token) (cl-conllu:token-id token)))))


(defun parent-distance-diff (g-dir p-dir)
  
    (cl-conllu::report-confusion-table #P"parent_distance/"
		 (directory g-dir)
		 (directory p-dir)
		 
		 #'token-to-parent-distance
		 (mapcar #'write-to-string (alexandria:iota 21 :start -10 :step 1))
 		 (mapcar #'write-to-string (alexandria:iota 21 :start -10 :step 1))

		 (lambda (columns)
		   (mapcar #'write-to-string (sort (mapcar #'parse-integer columns) #'> )))))


(defun parent-upostag-diff (g-dir p-dir)
    (cl-conllu::report-confusion-table "parent_upostag/"
		 (directory g-dir)
		 (directory p-dir)
		 #'parent-upostag))


(defun deprel-diff (g-dir p-dir)
    (cl-conllu::report-confusion-table "deprel/"
		 (directory g-dir)
		 (directory p-dir)
		 (lambda (token sent) (cl-conllu:token-deprel token))))


(defun run (g-dir p-dir)
  (deprel-diff g-dir p-dir)
  (parent-distance-diff g-dir p-dir)
  (parent-upostag-diff g-dir p-dir))


