;;;
;;; simple example of a post-op
;;;

;; these are used to find all function definitions in a sub-tree.
(defclass function-extractor ()
  ((functions :initform nil
	      :documentation "Stores the list of function-nodes found in a tree.")))

(defmethod c-mera::traverser ((ext function-extractor) (node cm-c::function-definition) level)
  (push node (slot-value ext 'functions))
  (call-next-method))

;post-op: anaphoric macro, which makes code accessible as 'tree' in the operation
(post-op
    ;code:
    (progn
      (function main() -> void
        (f))
      (function test() -> int))

    ;operation:
    (let ((f-ext (make-instance 'function-extractor)))
      (c-mera::traverser f-ext tree 0)
      (loop for f in (slot-value f-ext 'functions) do
        (set-name f 'another-name)))
    tree)
