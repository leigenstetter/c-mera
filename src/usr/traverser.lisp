(in-package :cm-usr)

(defvar traversers nil)

(defmethod add-traverser(traverser)
  (push traverser traversers))

(defclass cm-usr::usr-hook ()())

;entry point for user traversers
(defmethod c-mera::traverser ((hook cm-usr::usr-hook) (node c-mera::node) level)
  (loop for usr-traverser in traversers do
    (c-mera::traverser usr-traverser node level)))
