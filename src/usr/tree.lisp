(in-package :cmu-usr)


;;;
;;;
;;; In this file we provide helper functionality for using our
;;; generated (and at some points overly-nested) AST.
;;;
;;; NOTE that the AST interface is not considered entirely stable and
;;; there might be changes to it at some point.
;;;
;;;

(defmacro post-op (code &body body)
  "Proposed interface for AST-style transformations.  First, the
   c-mera CODE is evaluated and the BODY can then modify it via
   TREE. When the TREE is returned, the (potentially modified) tree is
   processed and evaluated.
   NOTE: macroexpansion will take place during evaluation!
   I.e. simply using it for analyzation might have side-effects."
  `(let ((tree ,code))
     ,@body))


;;;
;;; ast tools
;;;

(defmacro slot-value-chain (base last &rest rest)
  "this is a utility to follow through a chain of SLOT-VALUEs, i.e.
   (S-V-C x a b c) -> (S-V (S-V (S-V x a) b) c)
   quite helpful when navigating our (very verbose) tree structure"
  (cl:if rest
      (cl:let ((rev (cl:reverse rest)))
	`(slot-value (slot-value-chain ,base ,cl:last ,@(cl:reverse (cl:rest rev)))
		     ,(cl:first rev)))
      `(slot-value ,base ,cl:last)))

;;;
;;; find declaration names
;;;

(defgeneric decl-name (node)
  (:documentation "Find the names of declared variables (as given in
  FUNCTION and DECL forms."))

(defmethod decl-name ((node c-mera::identifier))
  (slot-value node 'c-mera::identifier))

(defmethod decl-name ((node  cm-c::function-definition))
  (decl-name (slot-value node 'c-mera:item)))

(defmethod decl-name ((node  cm-c::declaration-item))
  (decl-name (slot-value node 'c-mera::identifier)))

(defmethod decl-name ((node cm-c::source-position))
  (decl-name (slot-value node 'c-mera::subnode)))

(defmethod decl-name ((node cm-c::prefix-expression))
  (decl-name (slot-value node 'c-mera::object)))

(defmethod cmu-usr::decl-name ((node cm-usr::invisible-node))
  (slot-value node 'cm-usr::invisible-node))

(defmethod decl-name ((node cm-c::postfix-expression))
  (decl-name (slot-value node 'cm-c::object)))

(defmethod decl-name ((node cm-c::type))
  (decl-name (slot-value node 'cm-c::type)))

(defmethod decl-name ((node t))
  (format t "warning: no name method for this type ~a" node)
  nil)

;;;
;;; modify declaration names
;;;

(defgeneric set-name (node name)
  (:documentation "set the name of a declared variable"))

(defmethod set-name ((node c-mera::identifier) name)
  (setf (slot-value node 'c-mera::identifier) name))

(defmethod set-name ((node cm-c::function-definition) name)
  (set-name (slot-value node 'c-mera::item) name))

(defmethod set-name ((node cm-c::declaration-item) name)
  (set-name (slot-value node 'c-mera::identifier) name))

(defmethod set-name ((node cm-c::source-position) name)
  (set-name (slot-value node 'c-mera::subnode) name))

(defmethod set-name ((node cm-c::prefix-expression) name)
  (set-name (slot-value node 'c-mera::object) name))

(defmethod cmu-usr::set-name ((node cm-usr::invisible-node) name)
  (setf (slot-value node 'cm-usr::invisible-node) name))

(defmethod set-name ((node cm-c::postfix-expression) name)
  (set-name (slot-value node 'cm-c::object) name))

(defmethod set-name ((node cm-c::type) name)
  (set-name (slot-value node 'cm-c::type)name))

(defmethod set-name ((node cm-c::specifier) name)
  (setf (slot-value node 'specifier) name))

;;;
;;; find parameter names (of a function definition)
;;;

(defgeneric parameter-names (node))

(defmethod parameter-names ((node cm-c::function-definition))
  (let ((list (slot-value-chain node 'c-mera::parameter 'c-mera::parameters 'c-mera::nodes)))
    (loop for p in list collect (decl-name p))))
