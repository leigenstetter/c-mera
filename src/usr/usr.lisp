(in-package :cm-usr)

;; Define a c-mera file reader with extra macro characters
(define-reader
  :file-reader   read-in-file
  :string-reader read-in-string
  :macro-character
  ((set-macro-character #\Space #'cm-c::pre-process)
   (set-macro-character #\Tab #'cm-c::pre-process)
   (set-macro-character #\Newline #'cm-c::pre-process)
   (set-macro-character #\( #'cm-c::pre-process-heads))
  :pre-tree  '(pre-tree (make-nodelist nil))
  :post-tree  '(post-tree (make-nodelist nil)))

;; Define a start-up function
(define-processor
  :name usr-processor
  :file-reader   read-in-file
  :string-reader read-in-string
  :extra-traverser
  (nested-nodelist-remover
   else-if-traverser
   if-blocker
   decl-blocker
   renamer
   usr-hook))

 ;; Define a save function
(save-generator
 :name save
 :start-function usr-processor
 :in-package :cmu-usr)

;;; Define a reader switch with c++ pre-processing
(define-switch
  :name switch-reader
  :macro-character
  ((set-macro-character #\Space #'cm-c::pre-process)
   (set-macro-character #\Tab #'cm-c::pre-process)
   (set-macro-character #\Newline #'cm-c::pre-process)
   (set-macro-character #\( #'cm-c::pre-process-heads)))

(define-switches
  :cl-reader cl-reader
  :cm-reader cm-reader
  :macro-character
  ((set-macro-character #\Space #'cm-c::pre-process)
   (set-macro-character #\Tab #'cm-c::pre-process)
   (set-macro-character #\Newline #'cm-c::pre-process)
   (set-macro-character #\( #'cm-c::pre-process-heads)))