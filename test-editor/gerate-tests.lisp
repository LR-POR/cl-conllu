(ql:quickload :cl-conllu)

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
	  :acts ((> 5 4 3 2 1)
		 ("ra" 1 form "rando")
		 ("le" 1 form "la")
		 (last)))
	 (rule
	  :vars ((% feats "Gender=Masc")
		 (% misc "_"))
	  :rels ((> 1 2)
		 (distance 1 2 -100 100)
		 (not (= 1 2 form upostag)))
	  :acts ((> 2 1)
		 (+ 1 feats "Number=Plural||hello=oi")
		 (- 2 feats "Gender=Masc|Number=Sing|Dia=1|")
		 ("ard" 1 lemma "ando")
		 (set 1 form "olá")
		 (set 2 1 upostag form)
		 (set 2 form #'string-upcase))))))
  (with-open-file (str "test.txt"
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (prin1 (list (conllu.editor:conlluedit (cl-conllu:read-conllu #P"test.conllu") rule-1)
		 (conllu.editor:conlluedit (cl-conllu:read-conllu #P"test.conllu") rule-2))
	   str)))
