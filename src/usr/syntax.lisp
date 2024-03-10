(in-package :cm-usr)

(defmacro defsyntax (tags packages lambda-list &body body)
  (let ((tags (if (listp tags) tags (list tags))))
    `(progn
       ,@(loop for i in tags append
	   (loop for k in packages collect
	     `(let ((tag ',i))
		(declare (ignorable tag))
		(defmacro ,(intern (format nil "~:@(~a~)" i) k) ,lambda-list
		  ,@body)
		(export ',(intern (format nil "~:@(~a~)" i) k) ,k)))))))

(defmacro usr-syntax (tags packages lambda-list &body body)
  `(c-mera::defsyntax ,tags ,packages ,lambda-list ,@body))

(defmacro defusrnode (name values subnodes)
  (let ((package *package*))
  `(progn
     (in-package :cm-usr)
     (c-mera::defnode ,name ,values ,subnodes)
     (setf *package* ,package))))