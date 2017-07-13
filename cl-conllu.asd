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
  :version "0.3"
  :description "Common Lisp corpus conllu utilities"
  :author "Fabricio Chalub <fchalub@br.ibm.com> and Alexandre Rademaker <alexrad@br.ibm.com>"
  :license "Apache 2.0"

  :depends-on (#:cl-ppcre #:uuid #:alexandria #:cl-log #:split-sequence #:xmls #:yason)
  :components ((:file "packages")
	       (:file "data"          :depends-on ("packages"))
	       (:file "read-write"    :depends-on ("data"))
	       (:file "query"         :depends-on ("data"))
	       (:file "utils"         :depends-on ("data"))
	       (:file "rdf"           :depends-on ("data"))
	       (:file "command-line"  :depends-on ("data"))
	       (:file "rules"         :depends-on ("utils" "data"))
	       (:file "conllu-prolog" :depends-on ("data"))
	       (:file "palavras"      :depends-on ("data"))
	       (:file "tree"          :depends-on ("data"))))
