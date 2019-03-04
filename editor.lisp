
(in-package :conllu.editor)

(defun conlluedit (sentences rules)
  (reduce #'(lambda (acumulated-results sentence)
	      (let* ((sent-id (sentence-id sentence)) 
		     (results (apply-transformations sentence sent-id rules)))
		(if results
		    (cons (cons sent-id results) acumulated-results)
		    acumulated-results)))
	  sentences :initial-value nil))

;; part 0

(defun apply-transformations (sentence sent-id rules &optional (index 1) acumulated-results)
  (if rules
      (let* ((results (apply-transformation sentence sent-id (rest (first rules)) index))
	     (final-results (cons (cons index (rest results)) acumulated-results))) 
	(cond ((first results)
	       (apply-transformations sentence sent-id (rest rules)
				      (1+ index) final-results))
	      (results final-results)
	      (t (apply-transformations sentence sent-id (rest rules)
					(1+ index) acumulated-results))))
      acumulated-results))


(defun apply-transformation (sentence sent-id rule rule-index)
  (let* ((definitions    (definitions (getf rule :vars)))
	 (relations      (relations (getf rule :rels)))
	 (actions        (actions (getf rule :acts)))
	 (tokens         (sentence-tokens sentence))
	 (nodes          (node-matchers tokens definitions)))
    (when nodes
      (let ((sets (sets nodes relations)))
	(when sets 
	  (let ((merged-sets (merge-sets (length definitions) (length relations) sets)))
	    (when merged-sets
	      (let* ((result-actions (result-actions merged-sets tokens actions))
		     (final-actions (rest result-actions))) 
		(when (examine-actions final-actions) 
		  (let ((results (mapcar #'(lambda (result-action) (apply-action result-action))
 					 final-actions)))
		    (format *error-output* "Transformation ~a changed ~a tokens of Sentenca ~a ~%"
			    rule-index (length results) sent-id)
		    (cons (first result-actions) results)))))))))))


;; part 1

(defun definitions (definitions &optional (index 1) result-defs)
  (if definitions
      (let ((first-def (first definitions)))
	(definitions (rest definitions) (1+ index)
	  (cons (list :def-index index
		      :defs (mapcar #'def-match (if (listp (first first-def))
						    first-def
						    (list first-def))))
		result-defs)))
      result-defs))
  
(defun def-match (def)
  (let* ((negative (string= (first def) 'not))
	 (expression (if negative (nth 1 def) def))
	 (criterion (nth 1 expression))
	 (value (nth 2 expression)))
    (set-match-test (if (or (string= criterion 'id) (string= criterion 'head))
			(write-to-string value)
			value)
		    criterion negative (string= (first expression) '~))))
    
(defun set-match-test (value criterion negative-criterion regular-expression)
  #'(lambda (token)
      (let ((field (get-field-value criterion token)))
	(when field
	  (funcall (if negative-criterion #'not #'identity)
		   (if regular-expression
		       (cl-ppcre:scan value field)
		       (string= value field)))))))

(defun relations (relations)
  (mapcar #'relation
	  (reduce #'(lambda (result relation)
		      (append (normalize-shorthand relation '=) result))
		  relations :initial-value nil)))

(defun relation (relation)
  (let* ((negative (string= (first relation) 'not))
	 (expression (if negative (nth 1 relation) relation)))
    (list
     :defs (cons (nth 1 expression) (nth 2 expression))
     :relation-test (set-relation-test negative (first expression)
				       (nth 3 expression) (nth 4 expression)))))

(defun set-relation-test (negative first-expression field-1 field-2)
  #'(lambda (token-1 token-2)
      (let ((id-1 (parse-integer (get-field-value 'id token-1)))
	    (id-2 (parse-integer (get-field-value 'id token-2))))
	(and (not (= id-1 id-2))
	     (funcall (if negative #'not #'identity)
		      (if (string= first-expression '=)
			  (string= (get-field-value field-1 token-1)
				   (get-field-value field-2 token-2))
			  (let ((null-field-1 (if field-1 field-1 1)))
			    (<= null-field-1 (- id-2 id-1)
				(if field-2 field-2 null-field-1)))))))))

(defun actions (actions)
  (reverse (mapcar #'action
		   (reduce #'(lambda (result relation)
			       (append (normalize-shorthand relation 'set) result))
			   actions :initial-value nil))))

(defun action (expression)
  (unless (string= (first expression) 'last)
    (if (= (length expression) 5)
	(list t   (nth 1 expression) (nth 3 expression) (nth 2 expression) (nth 4 expression))
	(list nil (nth 1 expression) (nth 2 expression) (stringp (nth 3 expression)) (nth 3 expression)))))


(defun get-field-value (field token)
  (let ((value (slot-value token (intern (symbol-name field) "CL-CONLLU"))))
    (if (or (string= field 'id) (string= field 'head))
	(write-to-string value)
	value)))


(defun normalize-shorthand (expression operator)
  (if (string= (first expression) '>)
      (nth 1 (reduce #'(lambda (result node-2)
			 (list node-2
			       (cons `(,operator ,node-2 ,(first result) head id)
				     (nth 1 result))))
		     (cddr expression) :initial-value (list (nth 1 expression) nil)))
      (list expression)))


;; part 2

(defun node-matchers (tokens definitions &optional result-nodes)
  (if definitions
      (let* ((first-def (first definitions))
	     (tokens-matchers (token-matchers first-def tokens)))
	(when tokens-matchers
	  (node-matchers tokens (rest definitions)
			 (cons (cons (getf first-def :def-index) tokens-matchers)
			       result-nodes))))
      result-nodes))


(defun token-matchers (definition tokens)
  (reduce #'(lambda (result-matchers token)
	      (if (all-defs-test token (getf definition :defs))
		  (cons token result-matchers)
		  result-matchers))
	  tokens :initial-value nil))  


(defun all-defs-test (token defs) 
  (if defs
      (when (funcall (first defs) token)
	(all-defs-test token (rest defs)))
      t))


;;part 3

(defun sets (nodes relations &optional (relation-index 1) result-sets)
  (if relations
      (let ((set (relation-match nodes (first relations) relation-index)))
	(when set 
	  (sets nodes (rest relations) (1+ relation-index) (append set result-sets))))
      (if result-sets
	  result-sets
	  (mapcar #'(lambda (matcher-1)
		      (list :relation-index nil
			    :matchers (list (cons 1 (token-id matcher-1)))))
		  (rest (assoc 1 nodes))))))

(defun relation-match (nodes relation relation-index)
  (let* ((defs (getf relation :defs))
	 (def-1 (first defs))
	 (def-2 (rest defs)))
    (result-sets (getf relation :relation-test) relation-index def-1 def-2
		 (rest (assoc def-1 nodes)) (rest (assoc def-2 nodes)))))
 							    
(defun result-sets (relation-test relation-index def-1 def-2 nodes-1 nodes-2)
  (reduce #'(lambda (result-1 first-1)
	      (reduce #'(lambda (result-2 first-2)
			  (let ((set (result-set first-1 first-2 def-1 def-2
						 relation-test relation-index)))
			    (if set (cons set result-2) result-2)))
		      nodes-2 :initial-value result-1))
	  nodes-1 :initial-value nil))
  
(defun result-set (token-1 token-2 def-1 def-2 relation-test relation-index)
  (if (funcall relation-test token-1 token-2)
      (list :relation-index (list relation-index)
	    :matchers (list (cons def-1 (token-id token-1))
			    (cons def-2 (token-id token-2))))))


;; part 4  

(defun merge-sets (defs-count rels-count sets)
  (union-matchers defs-count rels-count
		  (reduce #'(lambda (result-sets set)
			      (let ((merged-matchers
				     (multiple-merges defs-count rels-count
						      set result-sets)))
				(append (list set) merged-matchers result-sets)))
			  sets :initial-value nil)))

(defun multiple-merges (defs-count rels-count bin-1 bins)
  (reduce #'(lambda (result-matchers bin)
	      (let ((merged-matchers (merge-matchers defs-count rels-count bin-1 bin)))
		(if merged-matchers
		    (cons merged-matchers result-matchers)
		    result-matchers)))
	  bins :initial-value nil))
		
(defun merge-matchers (defs-count rels-count bin-1 bin-2)
  (let ((matchers-union (union (getf bin-1 :matchers) (getf bin-2 :matchers)
			       :test #'tree-equal))
	(relation-indexes (union (getf bin-1 :relation-index)
				 (getf bin-2 :relation-index))))
    (when (and (<= (length matchers-union) defs-count)
	       (<= (length relation-indexes) rels-count))
      (list :relation-index relation-indexes
	    :matchers matchers-union))))

(defun union-matchers (defs-count rels-count sets)
  (reduce #'(lambda (result-sets set)
	      (let ((matcher (getf set :matchers)))
		(if (and (= (length (getf set :relation-index)) rels-count)
			 (= (length matcher) defs-count))
		    (union matcher result-sets)
		    result-sets)))
	  sets :initial-value nil))
  

;; part 5

(defun result-actions (sets tokens actions &optional result-actions)
  (cond ((null actions)
	 (cons t (reverse result-actions))) 
	((first actions)
	 (result-actions sets tokens (rest actions)
			     (cons (result-action sets tokens (first actions)) 
				   result-actions)))
	(t
	 (cons nil (reverse result-actions)))))

(defun result-action (sets tokens action)
  (let* ((field (nth 2 action))
	 (result-assocs (assocs (nth 1 action) sets field tokens)) 
	 (result-ids (first result-assocs))
	 (result-tokens (nth 1 result-assocs)) 
	 (old-fields (nth 2 result-assocs)))
    (list field result-tokens result-ids old-fields
	  (mapcar (cond ((first action)
			 #'(lambda (token)
			     (get-field-value (nth 4 action)
					      (nth (1- (rest (assoc (nth 3 action) sets)))
						   tokens))))
			((nth 3 action)
			 #'(lambda (token)
			     (nth 4 action)))
			(t
			 #'(lambda (token)
			     (eval `(funcall ,(nth 4 action) ,(get-field-value field token))))))
		  result-tokens))))

(defun assocs (value sets field tokens &optional ids result-tokens old-fields)
  (if sets
      (let ((set (first sets)))
	(if (= value (first set))
	    (let* ((id (rest set))
		   (token (nth (1- id) tokens))
		   (old-field (get-field-value field token)))
	      (assocs value (rest sets) field tokens (cons id ids)
		      (cons token result-tokens) (cons old-field old-fields)))
	    (assocs value (rest sets) field tokens ids result-tokens old-fields)))
      (list ids result-tokens old-fields)))

;; part 6	      

(defun examine-actions (final-actions &optional not-first)
  (if final-actions
      (let ((result-action (first final-actions)))
	(when (and (find (first result-action)
			 '(id form lemma upostag feats feats head deprel deps misc)
			 :test #'string=)
		   (not (string= 'nil (first (nth 4 result-action)))))
	  (examine-actions (rest final-actions) t)))
      not-first))

(defun apply-action (result-action)
  (let ((field (first result-action)))
    (mapcar (lambda (token token-id old-field string-value)
	      (let ((value (if (or (string= field 'id) (string= field 'head))
			       (parse-integer string-value)
			       string-value)))
		(progn
		  (setf (slot-value token (intern (symbol-name field) "CL-CONLLU")) value)
		  (list token-id field old-field value)))) 
	    (nth 1 result-action) (nth 2 result-action) (nth 3 result-action) (nth 4 result-action))))
