
(in-package :cl-conllu)

(defparameter *app* (make-instance 'ningle:<app>))

(djula:add-template-directory (asdf:system-relative-pathname "cl-conllu" "templates/"))

(defparameter +base.html+ (djula:compile-template* "base.html"))

(setf (ningle:route *app* "/hello/:name")
      #'(lambda (params)
	  (djula:render-template* +base.html+ nil
				  :title (cdr (assoc :name params))
				  :message "teste")))

(defparameter *my* (clack:clackup
		    (lack:builder
		     (:static :path (lambda (path)
				      (if (cl-ppcre:scan "^(?:/images/|/css/|/js/$)"
							 path)
					  path nil))
			      :root (asdf:system-relative-pathname "cl-conllu" "templates/"))
		     *app*)))

