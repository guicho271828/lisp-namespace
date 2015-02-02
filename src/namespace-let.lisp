
(in-package :lispn)

;; TODO namespace-let

;; renaming candidate:
;; namespace-let 
;; where --- eazy to type in, but incosistent with common lisp
;; bind --- I dont think it is cool, similar reason to where
;; overwriting cl:let --- well, maybe optional

(defmacro namespace-let (bindings &body body)
  (%pickone bindings body nil))

;; mutual recursion

(defun %pickone (bindings body acc)
  (ematch bindings
    (`(((function ,name) ,@definitions) ,@rest)
      (%nlet rest body (cons (flet-binding definitions) acc)))
    (`(((label ,name) ,@definitions) ,@rest)
      (%nlet rest body (cons (label-binding definitions) acc)))
    (`(((macro ,name) ,@definitions) ,@rest)
      (%nlet rest body (cons (macrolet-binding definitions) acc)))
    (`(((symbol-macro ,name) ,@definitions) ,@rest)
      (%nlet rest body (cons (symbol-macrolet-binding definitions) acc)))
    (`((,(and name (type symbol)) ,@definitions) ,@rest)
      (%nlet rest body (cons (let-binding definitions) acc)))
    (`((,(and else (satisfies bindingp)) ,@definitions) ,@rest)
      (%nlet rest body (cons (dispatch-binding definitions) acc)))
    (`()
     (render-form acc body))))

(defun %let (bindings body acc))
(defun %flet (bindings body acc))
(defun %macrolet (bindings body acc))
(defun %symbol-macrolet (bindings body acc))
(defun %namespace-let (bindings body acc))

