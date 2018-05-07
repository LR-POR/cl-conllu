;;;; HTML output
;;;; File for HTML outputs of objects in CL-CONLLU

;;; Usage overview:
;;
;; (format-html confusion-matrix)

(in-package :conllu.html)

(defparameter *confusion-matrix-style*
  "table, th, td {border: 1px solid black;border-collapse: collapse;padding: 5px;}
th, td {text-align: center;}
tr:first-child {color:blue; font-weight: bold;}
td:first-child { color:red; font-weight: bold;}
p {margin:0px;}
html * {font-family: Helvetica;}

table {overflow: hidden;}

tr:hover {background-color: #ffa;}

td, th {position: relative;}
td:hover::after,th:hover::after
{
  content: \"\";
  position: absolute;
  background-color: #ffa;
  left: 0;
  top: -5000px;
  height: 10000px;
  width: 100%;
  z-index: -1;
}"
  "HTML for styling the confusion matrix.")

(defgeneric format-html (object)
  (:documentation "Outputs a HTML string for the object."))

;;; for confusion matrices

(defun write-columns-headers (column-labels)
  "Auxiliary function for format-html for confusion-matrix."
  (mapcar
   (lambda (col)
     (cl-markup:markup
      (:td
       col)))
   column-labels))

(defun write-rows (row-labels column-labels cm)
  "Auxiliary function for format-html for the confusion-matrix CM."
  (mapcar
   (lambda (row)
     (cl-markup:markup 
      (:tr
       (:td row)
       (mapcar
        (lambda (column)
          (cl-markup:markup
           (:td
            (format nil "~a"
                    (conllu.evaluate:confusion-matrix-cell-count
                     row column cm
                     :default-if-undefined t)))))
        column-labels))))
   row-labels))

(defmethod format-html ((cm conllu.evaluate:confusion-matrix))
  (let ((cl-markup:*output-stream* nil)
        (columns (conllu.evaluate:confusion-matrix-columns-labels cm))
        (style *confusion-matrix-style*))
    (cl-markup:html
     (:style
      style)
     (:table
      (:tr (:td "")
           (write-columns-headers columns))
      (write-rows
       (conllu.evaluate:confusion-matrix-rows-labels cm)
       columns
       cm)))))
