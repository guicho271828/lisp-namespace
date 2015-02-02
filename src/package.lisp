
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
  (:use :cl :alexandria :introspect-environment :optima)
  (:nicknames :lispn)
  (:export :define-namespace
           :clear-namespace))

(in-package :lispn)

(defvar *namespaces* (make-hash-table :test 'eq))

(defun speed-required ()
  (< 2
     (second
      (assoc 'speed (declaration-information 'optimize)))))

(defun namespace-p         (name) (symbolp name))
(defun namespace-accessor  (name) (symbolicate "SYMBOL-" name))
(defun namespace-hash      (name) (symbolicate "*" name "-TABLE*"))
(defun namespace-condition (name) (symbolicate "UNBOUND-" name))
(defun namespace-boundp    (name) (symbolicate name "-BOUNDP"))

(defmacro define-namespace (name &optional (expected-type t))
  (when (member name '(function
                       macrolet
                       name
                       package
                       plist
                       value))
    (error "~a cannot be used as a namespace because it conflicts with the standard Common Lisp!"
           name))
  (ematch name
    ((namespace- accessor hash condition boundp)
     `(progn
        (defvar ,hash (make-hash-table :test 'eq))
        (define-condition ,condition (unbound-variable) ()
          (:report (lambda (c s) (format s "Symbol ~a is unbound in namespace ~a"
                                         (cell-error-name c) ',name))))
        (declaim (ftype (function (symbol) ,expected-type) ,accessor))
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
        (setf (gethash ',name *namespaces*) ',name)))))

;; (define-namespace menu function)

(defun clear-namespace (name &optional check-error)
  (when check-error
    (assert (gethash name *namespaces*)))
  (remhash name *namespaces*)
  (setf (symbol-value (symbolicate "*" name "-TABLE*"))
        (make-hash-table :test 'eq))
  name)


(defun bindingp (namespace-name)
  (gethash namespace-name *namespaces*))

