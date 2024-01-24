;;;
;;; namespaces for c functions
;;;

;class for a custom traverser

;implementation of the behavior



;(usr-syntax namespace (name &rest body)
;  `(cm-usr::namespace-node
;      (cm-c::make-node ,name)
;      (cm-c::make-nodelist ,body)))

;(cm-usr::usr-syntax namespace (name &rest body)
;  `(cmu-usr::namespace-node
;      (cm-c::make-node ,name)
;      (cm-c::make-nodelist ,body)))



;(c-syntax struct (name &body body)
;  "Struct definition"
;  `(struct-definition
;    ;; struct name
;    (make-node ,name)
;    ;; struct body
;    ,(when body
;      `(compound-statement
;       ;; curly braces: t
;       t
;       ;; build subnodes
;       (make-nodelist ,body)))))


;register custom traverser

(cl:progn
      #.(in-package :cm-usr) ;this switch has to happen at readtime, since c-mera reader is already active
      (defnode namespace-node () (identifier members))

      (defclass function-renamer ()())

	  (add-traverser (make-instance 'function-renamer))

      (usr-syntax namespace (name &rest body)
        `(namespace-node
            (make-node ,name)
            (make-nodelist ,body)))

      (in-package :cmu-usr)
      (export 'cmu-usr::namespace)

      (defmethod c-mera::traverser ((renamer function-renamer) (node cm-usr::namespace-node) level)
        (format t "~a~%" (cmu-usr::decl-name (slot-value node 'identifier)))
        (call-next-method))

      (defmethod c-mera::traverser ((renamer function-renamer) (node cm-c::function-definition) level)
              (cmu-usr::set-name node 'another-name) ;note that this happened after read-time (- is already replaced with _)
              (call-next-method)))

;anwendung: xml/json datenbank


;code
;(usr 'hallo)
;(function tst() -> void)

(namespace ns (function main() -> void))


