
(in-package :conllu.editor)

;;; Abbreviations:
;; prev    -> previous
;; sent(s) -> sentence(s)
;; acum    -> acumulated
;; def(s)  -> definitions(s)
;; rel(s)  -> relations(s)
;; act(s)  -> actions(s)
;; op      -> operator
;; fst     -> first
;; scd     -> second
;; rst     -> rest

(defun conlluedit (sentences rules)
  (reduce #'(lambda (prev-errors&records sentence)
              (let* ((prev-errors    (first prev-errors&records))
                     (prev-records   (rest prev-errors&records))
                     (sent-id        (sentence-id sentence))
                     (tokens         (sentence-tokens sentence))
                     (errors&records (apply-rules tokens sent-id rules prev-errors))
                     (errors         (first errors&records))
                     (records        (reverse (rest errors&records))))
                (cons errors (if records
                                 (cons (cons sent-id records) prev-records)
                                 prev-records))))
          sentences :initial-value nil))

;; part 0

(defun apply-rules (tokens sent-id rules errors &optional (index 1) acum-records)
  (if rules
      (if (find index errors)
          (apply-rules tokens sent-id (rest rules) errors (1+ index) acum-records)
          (let ((records (error-test (apply-rule tokens sent-id (rest (first rules)) index))))
            (if (integerp records)
                (apply-rules tokens sent-id (rest rules) (cons index errors) (1+ index) acum-records)
                (let ((last?         (first records))
                      (final-records (cons (cons index (rest records)) acum-records)))
                  (cond (last?
                         (apply-rules tokens sent-id (rest rules) errors (1+ index) final-records))
                        (records
                         (cons errors final-records))
                        (t
                         (apply-rules tokens sent-id (rest rules) errors (1+ index) acum-records)))))))
      (cons errors acum-records)))


(defmacro error-test (expression)
  `(let ((record (handler-case ,expression (error () 0))))
     (if (integerp record)
         (restart-case (error 'malformed-rule :index ,(fifth expression))
           (process-next-rules () 0))
         record)))


(define-condition malformed-rule (error)
  ((index :initarg :index :reader index))
  (:report (lambda (condition stream)
             (format stream "Error in rule: ~a~%Note: Previous rules may have been successfully completed.~&"
                     (index condition)))))


(defun apply-rule (tokens sent-id rule rule-index)
  (let* ((defs  (definitions (getf rule :defs)))
         (rels  (relations   (getf rule :rels)))
         (acts  (actions     (getf rule :acts)))
         (nodes (node-matchs tokens defs)))
    (when nodes
      (let ((sets (sets nodes rels)))
        (when sets
          (let ((merged-sets (merge-sets (length defs) (length rels) sets)))
            (when merged-sets
              (let* ((result-acts (result-acts merged-sets tokens acts))
                     (last?       (first result-acts))
                     (final-acts  (rest result-acts)))
                (when (examine-acts final-acts)
                  (let ((results (mapcar #'(lambda (result-act) (apply-act result-act)) final-acts)))
                    (format *error-output* "Transformation ~a made ~a changes in Sentenca ~a ~%" rule-index (length results) sent-id)
                    (cons last? results)))))))))))

;; part 1

(defun definitions (definitions &optional (index 1) result-defs)
  (if definitions
      (let ((fst-definition (first definitions)))
        (definitions (rest definitions) (1+ index)
          (cons (list :def-index index
                      :matchs (mapcar #'def-match (if (listp (first fst-definition))
                                                      fst-definition
                                                      (list fst-definition))))
                result-defs)))
      result-defs))


(defun def-match (def)
  (let* ((negative?  (string= (first def) 'not))
         (expression (if negative? (second def) def))
         (criterion  (second expression))
         (value      (third expression)))
    (match-test (if (or (string= criterion 'id) (string= criterion 'head))
                    (write-to-string value)
                    value)
                criterion negative? (first expression))))


(defun match-test (value criterion negative? op)
  #'(lambda (token)
      (let ((field (get-field-value criterion token)))
        (when field
          (funcall (if negative? #'not #'identity)
                   (cond ((string= op '=)
                          (string= value field))
                         ((string= op '~)
                          (cl-ppcre:scan value field))
                         ((string= op '%)
                          (find value (cl-ppcre:split "\\|" field) :test #'string=))))))))


(defun relations (relations)
  (mapcar #'relation
          (reduce #'(lambda (result relation)
                      (append (normalize-shortcut relation '=) result))
                  relations :initial-value nil)))


(defun relation (relation)
  (let* ((negative?  (string= (first relation) 'not))
         (expression (if negative? (second relation) relation)))
    (list :defs (cons (second expression) (third expression))
          :rel-test (relation-test negative? (first expression) (fourth expression) (fifth expression)))))


(defun relation-test (negative? op field-1 field-2)
  #'(lambda (token-1 token-2)
      (let ((id-1 (parse-integer (get-field-value 'id token-1)))
            (id-2 (parse-integer (get-field-value 'id token-2))))
        (and (/= id-1 id-2)
             (funcall (if negative? #'not #'identity)
                      (if (string= op '=)
                          (string= (get-field-value field-1 token-1)
                                   (get-field-value field-2 token-2))
                          (let* ((fst-field (if field-1 field-1 1))
                                 (scd-field (if field-2 field-2 fst-field)))
                            (<= fst-field (- id-2 id-1) scd-field))))))))


(defun actions (actions)
  (reverse
   (mapcar #'action
           (reduce #'(lambda (result action)
                       (append (normalize-shortcut action 'set) result))
                   actions :initial-value nil))))


(defun action (expression)
  (let ((op (first expression)))
    (unless (string= op 'last)
      (if (= (length expression) 5)
          (list op t (second expression) (fourth expression) (third expression) (fifth expression))
          (list op nil (second expression) (third expression) (stringp (fourth expression)) (fourth expression))))))


(defun get-field-value (field token)
  (let ((value (slot-value token (intern (symbol-name field) "CL-CONLLU"))))
    (if (or (string= field 'id) (string= field 'head))
        (write-to-string value)
        value)))


(defun normalize-shortcut (expression op)
  (if (string= (first expression) '>)
      (second
       (reduce #'(lambda (result node)
                   (list node
                         (cons `(,op ,node ,(first result) head id)
                               (second result))))
               (cddr expression) :initial-value (list (second expression) nil)))
      (list expression)))

;; part 2

(defun node-matchs (tokens defs &optional result-nodes)
  (if defs
      (let* ((fst-def       (first defs))
             (tokens-matchs (token-matchs fst-def tokens)))
        (when tokens-matchs
          (node-matchs tokens (rest defs)
                       (cons (cons (getf fst-def :def-index) tokens-matchs)
                             result-nodes))))
      result-nodes))


(defun token-matchs (def tokens)
  (reduce #'(lambda (result-matchs token)
              (if (defs-tests token (getf def :matchs))
                  (cons token result-matchs)
                  result-matchs))
          tokens :initial-value nil))


(defun defs-tests (token defs)
  (if defs
      (when (funcall (first defs) token)
        (defs-tests token (rest defs)))
      t))

;;part 3

(defun sets (nodes rels &optional (rel-index 1) result-sets)
  (if rels
      (let ((set (rel-match nodes (first rels) rel-index)))
        (when set
          (sets nodes (rest rels) (1+ rel-index) (append set result-sets))))
      (if result-sets
          result-sets
          (mapcar #'(lambda (match-def-1)
                      (list :rel-index nil
                            :matchs (list (cons 1 (token-id match-def-1)))))
                  (rest (assoc 1 nodes))))))


(defun rel-match (nodes rel rel-index)
  (let* ((defs  (getf rel :defs))
         (def-1 (first defs))
         (def-2 (rest defs)))
    (result-sets (getf rel :rel-test) rel-index def-1 def-2
                 (rest (assoc def-1 nodes)) (rest (assoc def-2 nodes)))))


(defun result-sets (rel-test rel-index def-1 def-2 nodes-1 nodes-2)
  (reduce #'(lambda (result-1 fst-1)
              (reduce #'(lambda (result-2 fst-2)
                          (let ((set (result-set fst-1 fst-2 def-1 def-2 rel-test rel-index)))
                            (if set
                                (cons set result-2)
                                result-2)))
                      nodes-2 :initial-value result-1))
          nodes-1 :initial-value nil))


(defun result-set (token-1 token-2 def-1 def-2 rel-test rel-index)
  (if (funcall rel-test token-1 token-2)
      (list :rel-index (list rel-index)
            :matchs (list (cons def-1 (token-id token-1))
                          (cons def-2 (token-id token-2))))))

;; part 4

(defun merge-sets (defs-count rels-count sets)
  (join-matchs defs-count rels-count
               (reduce #'(lambda (result-sets set)
                           (let ((merged-matchs (multiple-merges defs-count rels-count set result-sets)))
                             (append (list set) merged-matchs result-sets)))
                       sets :initial-value nil)))


(defun multiple-merges (defs-count rels-count bin-1 bins)
  (reduce #'(lambda (result-matchs bin)
              (let ((merged-matchs (merge-matchs defs-count rels-count bin-1 bin)))
                (if merged-matchs
                    (cons merged-matchs result-matchs)
                    result-matchs)))
          bins :initial-value nil))


(defun merge-matchs (defs-count rels-count bin-1 bin-2)
  (let ((matchs-union (union (getf bin-1 :matchs) (getf bin-2 :matchs) :test #'tree-equal))
        (rel-indexes  (union (getf bin-1 :rel-index) (getf bin-2 :rel-index))))
    (when (and (<= (length matchs-union) defs-count)
               (<= (length rel-indexes) rels-count))
      (list :rel-index rel-indexes
            :matchs matchs-union))))


(defun join-matchs (defs-count rels-count sets)
  (reduce #'(lambda (result-sets set)
              (let ((match (getf set :matchs)))
                (if (and (= (length (getf set :rel-index)) rels-count)
                         (= (length match) defs-count))
                    (union match result-sets)
                    result-sets)))
          sets :initial-value nil))

;; part 5

(defun result-acts (sets tokens acts &optional result-acts)
  (cond ((null acts)
         (cons t (reverse result-acts)))
        ((first acts)
         (result-acts sets tokens (rest acts) (cons (result-act sets tokens (first acts)) result-acts)))
        (t
         (cons nil (reverse result-acts)))))


(defun result-act (sets tokens act)
  (let* ((field         (fourth act))
         (result-assocs (assocs (third act) sets field tokens))
         (result-tokens (second result-assocs)))
    (list field result-tokens (first result-assocs) (third result-assocs)
          (mapcar #'(lambda (token)
                      (let ((value
                             (cond ((second act)
                                    (get-field-value (sixth act) (nth (1- (rest (assoc (fifth act) sets))) tokens)))
                                   ((fifth act)
                                    (sixth act))
                                   (t
                                    (eval `(funcall ,(sixth act) ,(get-field-value field token)))))))
                        (if (string= (first act) 'set)
                            value
                            (add-or-subt (first act) field token value))))
                  result-tokens))))


(defun assocs (value sets field tokens &optional ids result-tokens old-fields)
  (if sets
      (let ((set (first sets)))
        (if (= value (first set))
            (let* ((id        (rest set))
                   (token     (nth (1- id) tokens))
                   (old-field (get-field-value field token)))
              (assocs value (rest sets) field tokens (cons id ids) (cons token result-tokens) (cons old-field old-fields)))
            (assocs value (rest sets) field tokens ids result-tokens old-fields)))
      (list ids result-tokens old-fields)))


(defun add-or-subt (op field token value)
  (let ((token-value (get-field-value field token)))
    (clear
     (format nil "~{|~A~}"
             (reduce #'(lambda (result-value sub-value)
                         (cond ((string= op '-)
                                (remove sub-value result-value :test #'test-feats&misc))
                               ((string= op '+)
                                (if (find sub-value result-value :test #'test-feats&misc)
                                    (substitute sub-value sub-value result-value :test #'test-feats&misc)
                                    (append result-value (list sub-value))))))
                     (cl-ppcre:split "\\|" value) :initial-value (cl-ppcre:split "\\|" token-value))))))


(defun test-feats&misc (string-1 string-2)
  (string= (first (cl-ppcre:split "=" string-1))
           (first (cl-ppcre:split "=" string-2))))


(defun clear (string)
  (let ((length-string (length string)))
    (cond ((< length-string 2)
           "_")
          ((or (char= (elt string 0) #\|) (char= (elt string 0) #\_))
           (clear (subseq string 1)))
          ((or (char= (elt string (1- length-string)) #\|) (char= (elt string (1- length-string)) #\_))
           (clear (subseq string 0 (- length-string 1))))
          (t
           (cl-ppcre:regex-replace-all "\\|\\|" string "|")))))

;; part 6

(defun examine-acts (final-acts &optional not-fst)
  (if final-acts
      (let ((result-act (first final-acts)))
        (when (and (find (first result-act) '(id form lemma upostag xpostag feats head deprel deps misc) :test #'string=)
                   (not (string= 'nil (first (fifth result-act)))))
          (examine-acts (rest final-acts) t)))
      not-fst))


(defun apply-act (result-act)
  (let ((field (first result-act)))
    (mapcar (lambda (token token-id old-field string-value)
              (let ((value (if (or (string= field 'id) (string= field 'head))
                               (parse-integer string-value)
                               string-value)))
                (progn
                  (setf (slot-value token (intern (symbol-name field) "CL-CONLLU")) value)
                  (list token-id field old-field value))))
            (second result-act) (third result-act) (fourth result-act) (fifth result-act))))
