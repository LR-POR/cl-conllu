
(in-package :conllu.rdf)

(defun convert-features-to-rdf (features-string)
  "Input: string
   Output: list of feature nodes
   
   Returns a list of nodes to be used as objects in triples with the predicate \"conll:feats\".

   Examples:
   (let ((wilbur:*nodes*  (make-instance 'wilbur:dictionary))
         (wilbur:*db* (make-instance 'wilbur:db)))
     (wilbur:add-namespace \"olia-sys\" \"http://purl.org/olia/system.owl#\")
     (wilbur:add-namespace \"conll\" \"http://br.ibm.com/conll/LEMMA\")
     (convert-features-to-rdf \"Mood=Ind|Tense=Past|VerbForm=Fin\" (node \"sentence1\")))
    =>
   (!\"http://br.ibm.com/conll/LEMMA#verbFormFin\"
    !\"http://br.ibm.com/conll/LEMMA#tensePast\"
    !\"http://br.ibm.com/conll/LEMMA#moodInd\")"
  (if (equal features-string
	     "_")
      '()
      (mapcar
       #'produce-node-from-feat-value-pair
       (produce-feat-value-pairs-from-string features-string))))

(defun produce-feat-value-pairs-from-string (features-string)
  (mappend
   #'(lambda (feature-string)
       (destructuring-bind (feature-name values)
           (split-sequence #\= feature-string :count 2)
         (mapcar
          (lambda (value)
            (list feature-name value))
          (split-sequence #\, values))))
   (split-sequence #\| features-string
                   :remove-empty-subseqs nil)))

(defun produce-node-from-feat-value-pair (feat-value-pair)
  "Receives a list with two values (FEATURE-NAME FEATURE-VALUE) and returns a wilbur NODE for this feature with the conll prefix."
  (let ((feature-name (first feat-value-pair))
        (feature-value (second feat-value-pair)))
    (node (format nil "conll:~a~a"
                  (concatenate 'string
                               (string-downcase (subseq feature-name 0 1))
                               (encode-to-iri (subseq feature-name 1)))
                  feature-value))))

(defun convert-token-to-rdf (token sentence-id sentence-node)
  (let* ((token-node (node (format nil "NAMESPACE:~a-~a" sentence-id (slot-value token 'cl-conllu::id))))
	(slots '(id form lemma upostag xpostag feats head deprel deps))
	(slot-nodes
	 (list
	  'id `(,(wilbur:literal (slot-value token 'cl-conllu::id)))
	  'form `(,(wilbur:literal (slot-value token 'cl-conllu::form)))
	  'lemma `(,(wilbur:literal (slot-value token 'cl-conllu::lemma)))
	  'upostag `(,(wilbur:literal (slot-value token 'cl-conllu::upostag)))
	  'xpostag `(,(wilbur:literal (slot-value token 'cl-conllu::xpostag)))
	  'feats (convert-features-to-rdf (slot-value token 'cl-conllu::feats))
	  'head `(,(wilbur:literal (slot-value token 'cl-conllu::head)))
	  'deprel `(,(wilbur:literal (slot-value token 'cl-conllu::deprel)))
	  'deps `(,(wilbur:literal (slot-value token 'cl-conllu::deps))))))
    
    `(,(wilbur:triple token-node
		      (node "rdf:type")
		      (node "nif:Word"))
       ,(wilbur:triple token-node
		       (node "conll:inSentence")
		       sentence-node)
       ,@(mappend
	  #'(lambda (slot)
	      (mapcar
	       #'(lambda (value-node) 
		   (wilbur:triple
		    token-node
		    (node (format nil "conll:~a" (string-upcase slot)))
		    value-node))
	       (getf slot-nodes slot)))
	  slots))))

(defun convert-sentence-metadata (metadata sentence-node)
  "Input: list of pairs (name value), node
   Output: List of triples

   Example:
    (let ((wilbur:*db* (make-instance 'wilbur:db))
  		 (metadata '((\"sent_id\" . \"test\")
  			    (\"text\" . \"The US troops fired into the hostile crowd, killing 4.\")))
  		 (sentence-node (node \"sentence\")))
      (convert-sentence-metadata metadata sentence-node))
    =>
    ((#<WILBUR:TRIPLE !\"sentence\" !conll:metadata/sent_id #\"test\" {10048AA2E3}>)
     (#<WILBUR:TRIPLE !\"sentence\" !conll:metadata/text #\"The US troops fired into the hostile crowd, killing 4.\" {10048AA753}>))"
  (mapcar
   #'(lambda (pair)
       (cond
	 ((stringp (car pair))
	  (wilbur:triple
	   sentence-node
	   (node (format nil "conll:metadata/~a"
                         (encode-to-iri (car pair))))
	   (wilbur:literal (cdr pair))))
	 ((equal (car pair)
		 :raw)
	  (wilbur:triple
	   sentence-node
	   (node "conll:metadata")
	   (wilbur:literal (cdr pair))))
	 (t
	  (cerror "Ignore undetermined case."
		  "Indetermined metadata case: ~a" pair))))
   metadata))

(defun convert-sentence-to-rdf (sentence text sentence-id corpus-id-node)
  (let ((sentence-id-node (node
			   (format nil "NAMESPACE:~a" sentence-id))))
    `(,(wilbur:triple corpus-id-node
		      (node "conll:sentences")
		      sentence-id-node)
       
       ,(wilbur:triple sentence-id-node
		       (node "conll:corpus")
		       corpus-id-node)
       
       ,(wilbur:triple sentence-id-node
		       (node "rdf:type")
		       (node "conll:Sentence"))
       
       ,(wilbur:triple sentence-id-node
		       (node "rdfs:label")
		       (wilbur:literal text))
       ;; sentence metavalues
       ,@(convert-sentence-metadata (sentence-meta sentence)
				   sentence-id-node)
       
       ,@(mappend
	  #'(lambda (token)
	      (convert-token-to-rdf token sentence-id sentence-id-node))
	  (sentence-tokens sentence)))))
  

  
(defun convert-to-rdf (sentences &key (text-fn #'sentence-text) (id-fn #'sentence-id) (corpusname "my-corpus")
				   (namespace-string "http://www.example.org/") (stream *standard-output*)
				   (rdf-format :ntriples) (conll-namespace "http://br.ibm.com/conll/"))
  "Converts a list of sentences (e.g. as generated by READ-CONLLU)
   in SENTENCES, using the function TEXT-FN to extract the text of each
   sentence and ID-FN to extract the id of each sentence (we need this
   as there is no standardized way of knowing this.)
   Result is sent to the STREAM.

   Currently only ntriples is supported as RDF-FORMAT."

  (let* ((wilbur:*nodes* (make-instance 'wilbur:dictionary))
	 (wilbur:*db* (make-instance 'wilbur:db))	
	 (namespaces `(("conll"  ,conll-namespace)
		       ("rdf"  "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
		       ("rdfs"  "http://www.w3.org/2000/01/rdf-schema#")
		       ("dc"  "http://purl.org/dc/elements/1.1/")
		       ("dcterms"  "http://purl.org/dc/terms/")
		       ("skos"  "http://www.w3.org/2004/02/skos/core#")
		       ("owl"  "http://www.w3.org/2002/07/owl#")
		       ("nif"  "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#")
		       ("terms" "http://purl.org/acoli/open-ie/")
		       ("olia-sys" "http://purl.org/olia/system.owl#")
		       ("NAMESPACE" ,namespace-string))) ; "NAMESPACE" will be a prefix to represent local namespace
	 (corpus-id (uuid:make-v4-uuid))
	 (corpus-id-node (wilbur::unresolved-node (format nil "NAMESPACE:c~a" corpus-id))))

    ;; Inserts namespaces
    (dolist (namespace namespaces)
      (wilbur:add-namespace
       (first namespace)
       (second namespace)))
    ;; Produces and inserts triples
    (mapc #'wilbur:add-triple
	  `(,(wilbur:triple corpus-id-node
			    (node "rdf:type")
			    (node "conll:Corpus"))
	     ,(wilbur:triple corpus-id-node
			     (node "rdfs:label")
			     (wilbur:literal corpusname))
	     ,@(mappend
		#'(lambda (sentence)
		    (convert-sentence-to-rdf
		     sentence
		     (funcall text-fn sentence)
		     (funcall id-fn sentence)
		     corpus-id-node))
		sentences)))
    ;; Serializes
    ;; the same data can't be serialized by wilbur in both ntriples and rdf/xml.
    ;; thus, we are only using ntriples.
    (ecase rdf-format
      ;; ((:rdf/xml :xml)
      ;; 	(wilbur::dump-as-rdf/xml (reverse
      ;; 				  (wilbur:db-triples wilbur:*db*))
      ;; 				 stream
      ;; 				 (wilbur:namespaces)))
      (:ntriples 
       (wilbur::dump-as-ntriples (reverse
				  (wilbur:db-triples wilbur:*db*))
				 stream)))))


(defun node (arg)
  "Input: ARG
   Output: (node)
   Creates a new node. If ARG is a string, expands ARG with namespaces contained in
   dynamic variable *wilbur:nodes*"
  ;; (typecase arg
  ;;   (string
  ;;    (wilbur:node
  ;;     (wilbur:expand-name-with-namespace
  ;;      arg
  ;;      (wilbur:dictionary-namespaces wilbur:*nodes*))))
  ;;   (number
  ;;    (wilbur:node (format nil "~a" arg)))
  ;;   (t
  ;;    (wilbur:node arg))))
  (wilbur::unresolved-node arg))

(defun encode-to-iri (string)
  "Prepare a string in order to enter in a IRI"
  (quri.encode:url-encode string))

;; ==============
;; Some useful information/snippets about wilbur:
;; 
;; (setq *readtable* (copy-readtable nil)) ;; wilbur changes the
;; readtable, this reverts it (e.g. if having problems with
;; exclamation mark (!)

;; wilbur:add-namespace prefix uri 
;; (setf wilbur:*db* (make-instance 'wilbur:db))
;; (wilbur:triple (wilbur:node ...) (wilbur:node ...) (wilbur:node ...))
;; (wilbur:expand-name-with-namespace NAME-STRING (wilbur:dictionary-namespaces *nodes*))
;; (setf wilbur:*db* (make-instance 'wilbur:db))

;; Wilbur uses important dynamic variables:
;; *nodes* is special variable of class DICTIONARY that contains namespaces
;; *db* is a special variable corresponding to the current database
