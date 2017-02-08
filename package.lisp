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

(defpackage :cl-conllu
  (:use :cl :cl-ppcre :split-sequence)
  (:export
   #:sentence-meta-value
   #:sentence-valid?
   #:make-sentence
   #:sentence->text
   #:sentence-valid?
   #:read-conllu-from-stream
   #:read-conllu
   #:write-conllu
   #:write-conllu-to-stream   
   #:levenshtein
   #:diff
   #:insert-token
   #:remove-token

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

   #:mtoken
   #:mtoken-start
   #:mtoken-end
   #:mtoken-form

   #:sentence
   #:sentence-start
   #:sentence-meta
   #:sentence-tokens
   #:sentence-mtokens))
