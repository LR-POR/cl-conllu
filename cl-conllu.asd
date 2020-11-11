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


(asdf:defsystem #:cl-conllu
  :serial t
  :version "0.12"
  :description "Common Lisp library for dealing with CoNLL-U files"
  :long-description "This library provides a set of functions to work with CoNLL-U files. See https://universaldependencies.org/format.html for details about the CoNLL-U format adopted by the Universal Dependencies community. The library has functions for read/write files, apply rules for sentences transformation in batch mode, tree visualization, compare and evaluation trees etc. Documentation available in https://github.com/own-pt/cl-conllu/wiki."
  :author "Alexandre Rademaker <alexrad@br.ibm.com>"
  :license "Apache 2.0"
  :depends-on (#:cl-ppcre #:uuid #:alexandria #:cl-log #:split-sequence #:xmls
			  #:yason #:lispbuilder-lexer #:wilbur #:cl-markup :optima.ppcre)
  :components ((:file "packages")
	       (:file "data"               :depends-on ("packages"))
	       (:file "read-write"         :depends-on ("data"))
	       (:file "evaluate"           :depends-on ("data"))
               (:file "confusion-matrix"   :depends-on ("data"))
               (:file "html"               :depends-on ("data"))
	       (:file "query"              :depends-on ("data"))
	       (:file "utils"              :depends-on ("data"))
	       (:file "projective"         :depends-on ("utils"))
	       (:file "rdf"                :depends-on ("data"))
	       (:file "rdf-wilbur"         :depends-on ("data"))
	       (:file "command-line"       :depends-on ("data"))
	       (:file "rules"              :depends-on ("utils" "data"))
	       (:file "editor"             :depends-on ("utils" "data"))
	       (:file "conllu-prolog"      :depends-on ("data"))
	       (:file "niceline"           :depends-on ("data"))
	       (:file "tag-converter"      :depends-on ("data"))
	       (:file "draw"               :depends-on ("data"))))
