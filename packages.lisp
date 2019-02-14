;; Copyright 2016 IBM

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;;; package.lisp


(defpackage :cl-conllu
  (:use :cl :cl-ppcre :split-sequence :cl-log)
  (:export #:sentence-meta-value
	   #:sentence-valid?
	   #:make-sentence
	   #:sentence->text
	   #:read-stream
	   #:read-file
	   #:read-directory
	   #:read-conllu
           #:lazy-stream-reader
	   #:query
	   #:query-as-json
	   #:write-conllu
	   #:write-conllu-to-stream   
	   #:levenshtein
	   #:diff

	   #:sentence-binary-tree
         ; #:insert-token
         ; #:remove-token
	   #:adjust-sentence
	   #:non-projective?

	   #:token
	   #:token-id
	   #:token-form
	   #:token-lemma
	   #:token-upostag
	   #:token-xpostag
	   #:token-feats
	   #:token-head
	   #:token-deprel
	   #:token-deps
	   #:token-misc
	   #:token-sentence

           #:id
	   #:form
	   #:lemma
	   #:upostag
	   #:xpostag
	   #:feats
           #:head
	   #:deprel
           #:deps
	   #:misc
           #:simple-deprel

	   #:mtoken
	   #:mtoken-start
	   #:mtoken-end
	   #:mtoken-form

	   #:sentence
	   #:sentence-start
	   #:sentence-meta
	   #:sentence-hash-table
	   #:sentence-tokens
	   #:sentence-mtokens
	   #:sentence-text
	   #:sentence-id
	   #:sentence-equal
           #:sentence-size

	   #:convert-rdf
	   #:convert-rdf-file

	   #:convert-to-rdf))
  

(defpackage #:conllu.prolog
  (:use #:cl #:alexandria #:split-sequence #:cl-conllu)
  (:export #:convert-filename))

(defpackage #:conllu.rdf
  (:use #:cl #:wilbur #:alexandria #:cl-conllu #:split-sequence)
  (:shadowing-import-from #:cl-conllu
			  #:query
			  #:token)
  (:export #:convert-filename
	   #:convert-to-rdf))

(defpackage #:conllu.converters.niceline
  (:use #:cl #:cl-conllu #:lispbuilder-lexer))

(defpackage #:conllu.converters.tags
  (:use #:cl #:cl-conllu #:cl-ppcre)
  (:export #:write-sentence-tag-suffix-to-stream
	   #:write-sentences-tag-suffix-to-stream
	   #:write-sentences-tag-suffix
	   #:read-sentence-tag-suffix
	   #:read-file-tag-suffix))

(defpackage #:conllu.draw
  (:use #:cl #:cl-conllu)
  (:export #:tree-sentence))

(defpackage #:conllu.rules
  (:use #:cl #:cl-conllu)
  (:export #:apply-rules
	   #:apply-rules-from-files))

(defpackage #:conllu.editor
  (:use #:cl #:cl-conllu)
  (:documentation "ruled based transformations")
  (:export #:conlluedit))

(defpackage #:conllu.evaluate
  (:use #:cl #:cl-conllu)
  (:documentation "Functions for evaluating datasets and parser outputs in the CoNLL-U format.")
  (:export #:attachment-score-by-sentence
           #:attachment-score-by-word
           #:recall
           #:precision
           #:non-projectivity-accuracy
           #:non-projectivity-precision
           #:non-projectivity-recall
           #:exact-match
           #:exact-match-score
           
           #:confusion-matrix
           #:confusion-matrix-rows-labels
           #:confusion-matrix-columns-labels
           #:confusion-matrix-labels
           #:confusion-matrix-cells-labels
           #:confusion-matrix-cell-count
           #:confusion-matrix-cell-tokens
           #:confusion-matrix-corpus-id
           #:make-confusion-matrix
           #:confusion-matrix-update
           #:confusion-matrix-normalize))

(defpackage #:conllu.html
  (:use #:cl #:cl-conllu)
  (:documentation "Functions for producing html formatting of objects in the library.")
  (:export #:format-html
           #:*confusion-matrix-style*))
