;;;
;;; namespaces for c functions
;;;

;class for a custom traverser
(defclass function-renamer ()())

;implementation of the behavior
(defmethod c-mera::traverser ((renamer function-renamer) (node cm-c::function-definition) level)
  (set-name node 'another-name)
  (call-next-method))

;register custom traverser
(cm-usr::add-traverser (make-instance 'function-renamer))

;code
(function main() -> void)
