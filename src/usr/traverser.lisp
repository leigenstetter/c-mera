(in-package :cm-usr)

(defvar *traversers* nil)

(defmethod add-traverser(traverser)
  (push traverser *traversers*))

(defclass cm-usr::usr-hook ()())

;entry point for user traversers
(defmethod c-mera::traverser ((hook cm-usr::usr-hook) (node c-mera::node) level)
  (loop for usr-traverser in (reverse *traversers*) do
    (c-mera::traverser usr-traverser node level)))

;node that is not printed, allows for some simplifications
;similar concept as proxy
(defnode invisible-node (invisible-node) ())

;special nodes for beginning and end:
;user can add nodes at these points during traversal
(defnode pre-tree() (nodelist))
(defnode post-tree() (nodelist))
