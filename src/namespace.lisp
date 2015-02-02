
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

(defpackage :lisp-n
  (:use :cl :alexandria :introspect-environment)
  (:export :define-namespace
           :clear-namespace))

(in-package :lisp-n)

(defvar *namespaces* nil)

(defun speed-required ()
  (< 2
     (second
      (assoc 'speed (declaration-information 'optimize)))))

(defmacro define-namespace (name &optional (expected-type t))
  (when (member name '(function
                       macrolet
                       name
                       package
                       plist
                       value))
    (error "~a cannot be used as a namespace because it conflicts with the standard Common Lisp!"
           name))
  (let ((accessor (symbolicate "SYMBOL-" name))
        (hash (symbolicate "*" name "-TABLE*"))
        (condition (symbolicate "UNBOUND-" name))
        (boundp (symbolicate name "-BOUNDP")))
    `(progn
       (defvar ,hash (make-hash-table :test 'eq))
       (define-condition ,condition (unbound-variable) ()
         (:report (lambda (c s) (format s "Symbol ~a is unbound in namespace ~a"
                                        (cell-error-name c) ',name))))
       (declaim (ftype (function (symbol &optional (or null ,expected-type)) ,expected-type) ,accessor))
       (defun ,accessor (symbol)
         (multiple-value-bind (value found)
             (gethash symbol ,hash)
           (if found value
               (error ',condition :name symbol))))
       (defun ,boundp (symbol)
         (nth-value 1 (gethash symbol ,hash)))
       (declaim (ftype (function (,expected-type symbol) ,expected-type) (setf ,accessor)))
       (defun (setf ,accessor) (new-value symbol)
         ,@(if (speed-required)
               nil
               `((setf (get symbol 'name) new-value)))
         (setf (gethash symbol ,hash) new-value))
       ,@(when (speed-required)
               `((declare (inline ,accessor))
                 (declare (inline (setf ,accessor)))))
       (pushnew ',name *namespaces*))))

;; (define-namespace menu function)

(defun clear-namespace (name &optional check-error)
  (when check-error
    (assert (member name *namespaces*)))
  (removef *namespaces* name)
  (setf (symbol-value (symbolicate "*" name "-TABLE*"))
        (make-hash-table :test 'eq))
  name)

;; TODO namespace-let
