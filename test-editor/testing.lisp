(ql:quickload :cl-conllu)
(ql:quickload :prove)

(defpackage my-test
  (:use :cl
	:prove
	:cl-conllu))

(in-package :my-test)

(plan nil)

(let ((rule-1 
       '((rule
	  :vars (((= upostag "PROPN") (= deprel "flat:name"))
		 ((= upostag "PROPN") (= deprel "nmod"))
		 ((= upostag "NOUN") (= deprel "nsubj"))
		 (= upostag "PRON"))
	  :rels ((> 4 3 2 1))
	  :acts ((> 3 1)
		 (> 2 3)
		 (> 4 2)
		 (set 1 deprel "CASE")
		 (set 2 deprel "ADVERB")
		 (set 3 deprel "DET")
		 (set 4 deprel #'string-upcase)))))
      (rule-2
       '((rule
	  :vars ((~ id .*)
		 (not (= form "nothing"))
		 ((~ lemma ".*") (not (= form "nothing")))
		 ((~ head .*) (~ id .*) (~ form ".*"))
		 (not (= upostag ".*")))
	  :rels ((> 1 2 3 4 5))
	  :acts ((> 5 4 3 2 1) (last)))
	 (rule
	  :vars ((~ id .*)
		 (~ form ".*"))
	  :rels ((> 1 2)
		 (distance 1 2 -100 100)
		 (not (= 1 2 form upostag)))
	  :acts ((> 2 1)
		 (set 1 form "um")
		 (set 2 1 upostag form)
		 (set 2 form #'string-upcase))))))
  (is
   (with-open-file (str "test.txt")
     (read str))
   (list (conllu.editor:conlluedit (cl-conllu:read-conllu #P"test.conllu") rule-1)
	 (conllu.editor:conlluedit (cl-conllu:read-conllu #P"test.conllu") rule-2))
   :test #'(lambda (a b) (tree-equal a b :test (lambda (a b)
						 (cond ((integerp a) (= a b))
						       ((stringp a) (string= a b))
						       (t (eql a b))))))))
  
(finalize)
