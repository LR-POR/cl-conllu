
(in-package :conllu.editor)

(defun conlluedit (sentences transformations)
  (mapcar #'(lambda (sentence)
	      (all-transformations (sentence-tokens sentence) (sentence-id sentence)
				   transformations 1))
	  sentences)
  t)

;; part 0

(defun all-transformations (tokens sentence-index transformations index)
  (when transformations
    (when (apply-transformation tokens sentence-index (first transformations) index)
      (all-transformations tokens sentence-index (rest transformations) (1+ index)))))

(defun apply-transformation (tokens sentence-index transformation transform-index)
  (let* ((transformation (transformation transformation))
	 (definitions (getf transformation :definitions))
	 (relations (getf transformation :relations))
	 (actions (getf transformation :actions))
	 (nodes (node-matchers tokens definitions)))
    (when nodes
      (let ((sets (sets nodes relations)))
	(when sets 
	  (let ((set (merge-sets (list-length definitions) (list-length relations) sets)))
	    (when set
	      (let* ((result-actions (all-result-actions set tokens actions))
		     (final-actions (rest result-actions))) 
		(when (examine-actions final-actions) 
		  (mapcar #'(lambda (result-action) (apply-action result-action))
			  final-actions)
		  (format *error-output* "Sentence ~a changed by tranform ~a ~%"
			  sentence-index transform-index)
		  (first result-actions))))))))))


;; part 1

(defun transformation (transformation)
  (list :definitions (definitions (first transformation)) 
	:relations (relations (nth 1 transformation))
	:actions (actions (nth 2 transformation))))

(defun definitions (definitions &optional (index 1) (result-defs nil))
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
  (let* ((negative (eql (first def) 'not))
	 (expression (if negative (nth 1 def) def))
	 (criterion (nth 1 expression))
	 (value (nth 2 expression)))
    (set-match-test (if (eql criterion 'id) (write-to-string value) value)
		    criterion negative (not (eql (first expression) '=)))))
    
(defun set-match-test (value criterion negative-criterion regular-expression)
  #'(lambda (token)
      (let ((field (get-field criterion token)))
	(when field
	  (funcall (if negative-criterion #'not #'identity)
		   (if regular-expression
		       (not (not (cl-ppcre:scan value field)))
		       (string= value field)))))))

(defun relations (relations)
  (mapcar #'relation
	  (reduce #'(lambda (result relation)
		      (append (normalize-shorthand relation '=) result))
		  relations :initial-value nil)))

(defun relation (relation)
  (let* ((negative (eql (first relation) 'not))
	 (expression (if negative (nth 1 relation) relation)))
    (list
     :defs (cons (nth 1 expression) (nth 2 expression))
     :relation-test (set-relation-test negative (first expression)
				       (nth 3 expression) (nth 4 expression)))))

(defun set-relation-test (negative first-expression field-1 field-2)
  #'(lambda (token-1 token-2)
      (let ((id-1 (parse-integer (get-field 'id token-1)))
	    (id-2 (parse-integer (get-field 'id token-2))))
	(and (not (= id-1 id-2))
	     (funcall (if negative #'not #'identity)
		      (if (eql first-expression '=)
			  (string= (get-field field-1 token-1)
				   (get-field field-2 token-2))
			  (let ((null-field-1 (if field-1 field-1 1)))
			    (<= null-field-1 (- id-2 id-1)
				(if field-2 field-2 null-field-1)))))))))

(defun actions (actions)
  (reverse (mapcar #'action
		   (reduce #'(lambda (result relation)
			       (append (normalize-shorthand relation 'set) result))
			   actions :initial-value nil))))

(defun action (expression)
  (unless (eql (first expression) 'last)
    (let ((nth-1 (nth 1 expression))
	  (nth-2 (nth 2 expression))
	  (nth-3 (nth 3 expression)))
      (if (= (list-length expression) 5)
	  (list t nth-1 nth-3 nth-2 (nth 4 expression))
	  (list nil nth-1 nth-2 (stringp nth-3) nth-3)))))

(defun get-field (field token)
  (cond ((string= field 'id) (write-to-string (token-id token)))
	((string= field 'form) (token-form token))
	((string= field 'lemma) (token-lemma token))
	((string= field 'upostag) (token-upostag token))
	((string= field 'feats) (token-feats token))
	((string= field 'head) (write-to-string (token-head token)))
	((string= field 'deprel) (token-deprel token))
	((string= field 'deps) (token-deps token))
	((string= field 'misc) (token-misc token))))

(defun normalize-shorthand (expression operator)
  (if (eql (first expression) '>)
      (nth 1 (reduce #'(lambda (result node-2)
			 (list node-2
			       (cons `(,operator ,node-2 ,(first result) head id)
				     (nth 1 result))))
		     (cddr expression) :initial-value (list (nth 1 expression) nil)))
      (list expression)))


;; part 2

(defun node-matchers (tokens definitions &optional (result-nodes nil))
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
	      (let ((token-matched (token-match definition token)))
		(if token-matched
		    (cons token-matched result-matchers)
		    result-matchers)))
	  tokens :initial-value nil))  

(defun token-match (definition token)
  (when (all-defs-test token (getf definition :defs))
    token))

(defun all-defs-test (token defs) 
  (if defs
      (when (funcall (first defs) token)
	(all-defs-test token (rest defs)))
      t))


;;part 3

(defun sets (nodes relations &optional (relation-index 1) (result-sets nil))
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
    (when (and (<= (list-length matchers-union) defs-count)
	       (<= (list-length relation-indexes) rels-count))
      (list :relation-index relation-indexes
	    :matchers matchers-union))))

(defun union-matchers (defs-count rels-count sets)
  (reduce #'(lambda (result-sets set)
	      (let ((matcher (getf set :matchers)))
		(if (and (= (list-length (getf set :relation-index)) rels-count)
			 (= (list-length matcher) defs-count))
		    (union matcher result-sets)
		    result-sets)))
	  sets :initial-value nil))
  

;; part 5

(defun all-result-actions (sets tokens actions &optional (result-actions nil))
  (cond ((null actions)
	 (cons t (reverse result-actions))) 
	((first actions)
	 (all-result-actions sets tokens (rest actions)
			     (cons (result-action sets tokens (first actions)) 
				   result-actions)))
	(t
	 (cons nil (reverse result-actions)))))

(defun result-action (sets tokens action)
  (let ((result-tokens (assoc-tokens (nth 1 action) tokens sets))
	(field (nth 2 action)))
    (list field result-tokens
	  (mapcar (cond ((first action)
			 #'(lambda (token)
			     (get-field (nth 4 action)
					(nth (1- (rest (assoc (nth 3 action) sets)))
					     tokens))))
			((nth 3 action)
			 #'(lambda (token)
			     (nth 4 action)))
			(t
			 #'(lambda (token)
			     (eval `(funcall ,(nth 4 action) ,(get-field field token))))))
		  result-tokens))))

(defun assoc-tokens (value tokens sets)
  (reduce #'(lambda (result-tokens set)
	      (if (= value (first set))
		  (cons (nth (1- (rest set)) tokens) result-tokens)
		  result-tokens))
	  sets :initial-value nil))


;; part 6	      

(defun examine-actions (final-actions &optional (not-first nil))
  (if final-actions
      (let ((result-action (first final-actions)))
	(when (and (find (first result-action)
			 '(id form lemma upostag feats feats head deprel deps misc)
			 :test #'string=)
		   (not (string= 'nil (first (nth 2 result-action)))))
	  (examine-actions (rest final-actions) t)))
      not-first))

(defun apply-action (result-action)
  (let ((field (first result-action)))
    (mapcar (lambda (token string-value)
	      (let ((value (if (or (string= field 'id) (string= field 'head))
			       (parse-integer string-value)
			       string-value)))
		(setf (slot-value token (itern field "CL-CONLLU")) value)))
	    (nth 1 result-action) (nth 2 result-action))))

