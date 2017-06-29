
;; File for extracting semantic roles from the Misc field to a RDF file.
;; (issue #30)

;; A first implementation will consider PALAVRAS' dependencies.

;; (ql:quickload :cl-ppcre)

(ql:quickload :wilbur)
(setq *readtable* (copy-readtable nil)) ;; wilbur changes the readtable, this reverts it

;; (wilbur:add-namespace "plvrs" "http://example.org/palavras/")
;; (wilbur:add-namespace "dhbb" "http://example.org/dhbb/")

;; In the MISC field, when some key has multiple values, we'll separete it by commas (",").
;; For instance:
;; Role=Agent;Sem=human,person

(defparameter *misc-field-separator* ",")
(defparameter *instance-relationship* "rdf:type")

(setf wilbur:*db* (make-instance 'wilbur:db))

(defclass vertex ()
  ((id :initarg :id
       :accessor vertex-id)
   (name :initarg :name
	 :accessor vertex-name)
   (head :initarg :head
	 :accessor vertex-head)
   (role :initarg :role
	 :accessor vertex-role)
   (sem :initarg :sem
	:accessor vertex-sem)))

(defmethod print-object ((vert vertex) stream)
  (print-unreadable-object (vert stream :type 'vertex)
    (format stream "~s" (vertex-name vert))))

(defun extract-roles (sentences &key (relation-namespace "ns") (collect-heads t))
  (mapcar
   #'(lambda (sentence)
       (multiple-value-call
	   #'(lambda (vertex-list triple-list)
	       (add-relations
		  vertex-list
		  triple-list
		  :collect-heads collect-heads
		  :relation-namespace relation-namespace))
	 (add-sem
	  (sentence-to-vertex-list sentence))))
   sentences))

(defun sentence-to-vertex-list (sentence)
  ;; malt-sentences -> vertex-list
  (let ((vertex-list nil))
    (push
     (make-instance 'vertex
		    :id 0
		    :name "0-root"
		    :head 0
		    :role "_"
		    :sem (list "_"))
     vertex-list)
    (dolist (word (sentence-tokens sentence)
	     vertex-list)
      (with-slots (id form head) word
	(push 
	 (make-instance 'vertex
			:id id
			:name (concatenate 'string
					   (format nil "~a" id)
					   "-"
					   form)
			:head head
			:role (token-misc-value word "role")
			:sem (cl-ppcre:split
			      *misc-field-separator*
			      (token-misc-value word "sem")))
	 vertex-list)))))

(defun add-sem (vertex-list)
  ;; vertex-list -> (vertex-list, triple-list)
  (let ((triple-list nil))
    (dolist (vert vertex-list)
      (with-slots (sem) vert
	(dolist (sem-element sem)
	  (if (not (null sem-element))
	      (push
	       (wilbur:triple
		(wilbur:node (vertex-name vert))
		(wilbur:node *instance-relationship*)
		(wilbur:node sem-element))
	       triple-list)))))
    (values
     vertex-list
     triple-list)))

(defun add-relations (vertex-list triple-list &key (relation-namespace "ns") (collect-heads t))
  ;; vertex-list, triple-list -> triple-list
  ;; returns triple-list
  ;; We are considering "valid vertex" words that have role or extra.
  ;; If a word's head isn't a "valid vertex", we move one level upwards
  (labels
      ((valid? (vertex)
	 (if (and (equal (vertex-role vertex)
			 nil)
		  (equal (vertex-sem vertex)
			 nil)
		  (not (equal (vertex-id vertex)
			      0)))                
	     nil
	     t))
       (fix-vertex (vertex)
	 ;; Goes upward on dependency relations in order to make
	 ;; triples only with 'valid' vertices, modifying the vertex
	 ;; object.
	 ;; Returns the "final" head.
	 (let ((head (find (vertex-head vertex)
			   vertex-list
			   :key #'vertex-id
			   :test #'equal)))
	   (if (null head)
	       (error "Null head"))
	   (if (valid? head)
	       head
	       (progn
		 (if collect-heads
		     (setf (vertex-name vertex)
			   (if (> (vertex-id vertex)
				  (vertex-id head))
			       (concatenate
				'string
				(vertex-name head)
				"-"
				(vertex-name vertex))
			       (concatenate
				'string
				(vertex-name vertex)
				"-"
				(vertex-name head)))))
		 (setf (vertex-head vertex)
		       (vertex-head head))
		 (fix-vertex vertex))))))
    (dolist (vert vertex-list triple-list)
      (when (and
	     (not (equal
		   (vertex-role vert)
		   nil))
	     (not (equal
		   "0"
		   (vertex-head vert))))
	(let ((head-vertex (fix-vertex vert)))
	  (push (wilbur:triple
		 (wilbur:node (vertex-name vert))
		 (wilbur:node (concatenate
			       'string
			       relation-namespace
			       ":"
			       (vertex-role vert)))
		 (wilbur:node (vertex-name head-vertex)))
		triple-list))))))

