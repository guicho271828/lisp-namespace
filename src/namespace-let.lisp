
(in-package :lispn)

;; TODO namespace-let

;; renaming candidate:
;; namespace-let 
;; where --- eazy to type in, but incosistent with common lisp
;; bind --- I dont think it is cool, similar reason to where
;; overwriting cl:let --- well, maybe optional

(named-readtables:in-readtable :fare-quasiquote)

(defmacro namespace-let (bindings &body body)
  (%pickone (reverse bindings) `((progn ,@body))))

;; mutual recursion

(defun %pickone (bindings body)
  (ematch bindings
    ;; function-like binding
    (`(((function ,name) ,@definition) ,@rest)
      (%merge 'flet name definition body rest))
    (`(((label ,name) ,@definition) ,@rest)
      (%merge 'labels name definition body rest))
    (`(((macro ,name) ,@definition) ,@rest)
      (%merge 'macrolet name definition body rest))
    ;; variable-like binding
    (`(((symbol-macro ,name) ,@definition) ,@rest)
      (%merge 'symbol-macrolet name definition body rest))
    (`((,(and name (type symbol)) ,@definition) ,@rest)
      (%merge 'let name definition body rest))
    ;; handler binding
    (`(((handler ,name) ,@definition) ,@rest)
      (%merge 'handler-bind name definition body rest))
    (`(((restart ,name) ,@definition) ,@rest)
      (%merge 'restart-bind name definition body rest))
    ;; namespace binding
    (`(((,(and namespace (binding)) ,name) ,@definition) ,@rest)
      (%pickone rest (%wrap namespace name definition body)))
    (`() `(progn ,@body))))

(defun %merge (kind name def body rest)
  (%pickone
   rest
   (match body
     (`((,(eq kind) (,@bindings) ,@body))
      `((,kind ((,name ,@def) ,@bindings) ,@body)))
     (_
      `((,kind ((,name ,@def)) ,@body))))))

(defun %wrap (namespace name definition body)
  (ematch namespace
    ((namespace- accessor)
     (with-gensyms (temp)
       `((let ((,temp ,@definition))
           (macrolet ((,accessor (&whole whole x)
                        (if (equal x '(quote ,name))
                            ',temp
                            whole)))
             ,@body)))))))

(named-readtables:in-readtable nil)

