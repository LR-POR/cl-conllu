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

;;;; conll-prolog.lisp

(in-package #:conll-prolog)

(defun toprologid (str)
  (cl-ppcre:regex-replace-all "[^A-Za-z0-9_]"
			      (string-downcase (string-trim '(#\- #\. #\, #\; #\` #\' #\space)
							    (format nil "~A" str))) "_"))

(defun make-id (context prefix id)
  (unless id
    (setf id (uuid:make-v4-uuid)))
  (if context 
      (format nil "c~a_~a~a" (toprologid context) (toprologid prefix) (toprologid id))
      (format nil "~a~a" (toprologid prefix) (toprologid id))))

(defun is-root (str)
  (string-equal str "root"))

(defun clean-dep-rel (str)
  (cl-ppcre:regex-replace-all "[^A-Za-z0-9_]"
			      (string-downcase (string-trim '(#\- #\. #\, #\; #\` #\' #\space) str)) "_"))

(defparameter *clauses* nil)

(defparameter *dependencies* nil)

(defun emit-prolog (clause text)
  (push text (gethash clause *clauses*)))

(defun write-prolog (out)
  (format out "%% -*- prolog -*-~%")
  (format out ":- dynamic nlp_dependency/4.~%")
  (maphash (lambda (k clauses) 
             (format out "~%")
             (format out "%% ~a~%" k)
             (dolist (c clauses) (format out "~a~%" c))) *clauses*)
  (maphash (lambda (k v)
             (format out "nlp_~a(S,T1,T2) :- nlp_dependency(S,T1,T2,~a).~%" k v))
           *dependencies*))

(defun prolog-string (str &optional (downcase t))
  (format nil "'~a'" (cl-ppcre:regex-replace-all "([\'\\\\])" (if downcase (string-downcase str) str) '("\\" :match))))

(defun process-features (sentence-id word-index-id feats-str)
  (let ((features (split-sequence #\| feats-str)))
    (dolist (feat features)
      (let ((k-v (split-sequence #\= feat)))
        (when (= 2 (length k-v))
          (emit-prolog (car k-v) (format nil "nlp_feat_~a(~a,~a,~a)." (car k-v)  sentence-id word-index-id (prolog-string (cadr k-v) nil))))
        (when (= 1 (length k-v))
          (emit-prolog (car k-v) (format nil "nlp_feat_~a(~a,~a)." (car k-v) sentence-id word-index-id)))))))

(defun process-tokens (context sentence-id token)
  (let ((word-index-id (make-id context "i" (token-id token)))
        (dep-rel (token-deprel token))
        (misc (mapcar (lambda (x) (split-sequence #\= x)) (split-sequence #\| (token-misc token))))
        (head-id (make-id context "i" (token-head token))))

    (when (> (length misc) 0)
      (let ((sense (assoc "FlSense" misc :test #'equal)))
        (when (and sense (not (equal "?" (cadr sense))))
          (emit-prolog "sense" (format nil "nlp_sense(~a,~a,~a)."
				       sentence-id word-index-id (prolog-string (cadr sense)))))))
    (emit-prolog "dependency" (format nil "nlp_dependency(~a,~a,~a,~a)."
				      sentence-id word-index-id head-id (prolog-string dep-rel)))
    (process-features sentence-id word-index-id (token-feats token))
    (process-features sentence-id word-index-id (token-misc token))
    (emit-prolog "idx" (format nil "nlp_index(~a,~a,~a)." sentence-id word-index-id (token-id token)))
    (emit-prolog "form" (format nil "nlp_form(~a,~a,~a)." sentence-id word-index-id (prolog-string (token-form token) nil)))
    (emit-prolog "lemma" (format nil "nlp_lemma(~a,~a,~a)." sentence-id word-index-id (prolog-string (token-lemma token))))
    (emit-prolog "pos" (format nil "nlp_pos(~a,~a,'~a')." sentence-id word-index-id (token-upostag token)))
    (if (is-root dep-rel)
        (emit-prolog "root" (format nil "nlp_sent_root(~a,~a)." sentence-id word-index-id))
        (unless (gethash (clean-dep-rel dep-rel) *dependencies*)
          (setf (gethash (clean-dep-rel dep-rel) *dependencies*) (prolog-string dep-rel))))))

(defun clean-whitespace (line)
  (string-trim '(#\space #\tab) line))

(defun valid-line (line)
  (> (length line) 0))

(defun convert-filename (context filename-in filename-out)
  (setf *clauses* (make-hash-table :test #'equal))
  (setf *dependencies* (make-hash-table :test #'equal))
  (let ((sentences (read-conllu filename-in)))
    (dolist (s sentences)
      (let ((sid (make-id context "s" (sentence-meta-value s "sent_id"))))
        (dolist (metadata (sentence-meta s))
          (emit-prolog (format nil "sentence_~a" (car metadata)) (format nil "nlp_sentence_~a(~a,~a)." (car metadata) sid (prolog-string (cdr metadata) nil))))
        (emit-prolog "sentence" (format nil "nlp_sentence(~a)." sid))
        (dolist (tk (sentence-tokens s))
          (process-tokens context sid tk)))))
  (with-open-file (fout filename-out :direction :output :if-exists :supersede)
    (write-prolog fout)))

;;
;; (convert-filename "sample" "repos/conll-prolog/complex.conll" "complex.pl")
;;
