
#|

This file provide additional namespace for lisp.

Common lisp is lisp-2, which means it has a different namespaces for the
value and the function. With lisp-n, you can define arbitrary additional
namespaces and its accessors as well.

The idea is simple.  Common lisp has `symbol-value' and `symbol-function',
so I added `symbol-anything-you-like'.  Current implementation is
built upon a hashtable, but it also modifies `cl:symbol-plist', for the
debugging purpose. I assume there won't be so many additional namespaces.

|#

(defpackage :lisp-namespace
  (:use :cl :alexandria :optima)
  (:nicknames :lispn)
  (:export :define-namespace
           :clear-namespace
           :namespace-let
           :nslet))

(in-package :lispn)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; the name of this variable shoud not be changed, to maintain consistency
  ;; to the hash tables defined by define-namespace.
  (defvar *namespace-hash* (make-hash-table :test 'eq))
  (defun %namespace-p         (name) (symbolp name))
  (defun %namespace-accessor  (name) (symbolicate "SYMBOL-" name))
  (defun %namespace-hash      (name) (symbolicate "*" name "-TABLE*"))
  (defun %namespace-condition (name) (symbolicate "UNBOUND-" name))
  (defun %namespace-boundp    (name) (symbolicate name "-BOUNDP"))
  (defun %namespace-type      (name) (symbolicate name "-TYPE"))
  (defun %namespace-letname   (name) (symbolicate name "-LET")))

(defmacro define-namespace (name &optional
                                   (expected-type t)
                                   (binding t))
  (when (member name '(function
                       macrolet
                       name
                       package
                       plist
                       value))
    (error "~a cannot be used as a namespace because it conflicts with the standard Common Lisp!"
           name))
  (ematch name
    ((%namespace- accessor hash condition boundp letname type)
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (defvar ,hash (make-hash-table :test 'eq))
        (define-condition ,condition (unbound-variable) ()
          (:report (lambda (c s) (format s "Symbol ~a is unbound in namespace ~a"
                                         (cell-error-name c) ',name))))
        (deftype ,type () ',expected-type)
        (declaim (ftype (function (symbol) (,type)) ,accessor))
        (declaim (ftype (function ((,type) symbol) (,type)) (setf ,accessor)))
        (declaim (inline ,accessor))
        (declaim (inline (setf ,accessor)))
        (defun ,accessor (symbol)
          (multiple-value-bind (value found)
              (gethash symbol ,hash)
            (if found value
                (error ',condition :name symbol))))
        (defun ,boundp (symbol)
          (nth-value 1 (gethash symbol ,hash)))
        (defun (setf ,accessor) (new-value symbol)
          (setf (gethash symbol ,hash) new-value))
        ,(when binding
           `(defmacro ,letname (bindings &body body)
              `(namespace-let
                   ,(mapcar (lambda (bind) `((,',name ,(car bind)) ,@(cdr bind)))
                            bindings)
                 ,@body)))
        (setf (gethash ',name *namespace-table*) ',name)))))

(define-namespace namespace symbol nil)

(defun clear-namespace (name &optional check-error)
  (when check-error
    (assert (gethash name *namespace-table*)))
  (remhash name *namespace-table*)
  (setf (symbol-value (%namespace-hash name))
        (make-hash-table :test 'eq))
  name)
