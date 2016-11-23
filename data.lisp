
(in-package :cl-conllu)


(defclass token ()
  (id form lemma upostag xpostag feats head deprel deps misc))


(defclass sentence ()
  ((start  :initarg :start
	   :initform 0
	   :accessor sentence-start)
   (meta   :initarg :meta
	   :initform nil
	   :accessor sentence-meta)
   (tokens :initarg :tokens
	   :initform nil
	   :accessor sentence-tokens)))
