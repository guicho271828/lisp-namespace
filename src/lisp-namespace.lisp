;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(uiop:define-package #:lisp-namespace
  (:use #:cl)
  (:export #:define-namespace
           #:clear-namespace))

(in-package #:in-nomine)

(defmacro lisp-namespace:define-namespace (name &rest args)
  (when (keywordp (first args))
    (error "Unsupported ~S form. (Did you mean to use ~S instead?)"
           'lisp-namespace:define-namespace
           'define-namespace))
  (destructuring-bind
      (&optional (value-type 't) (letp t letpp) documentation) args
    (when (and letpp letp) (short-form-warn-letp))
    (let ((table-symbol (symbolicate '#:* name '#:-table*))
          (doc-table-symbol (symbolicate '#:* name '#:-doc-table*)))
      `(define-namespace ,name
         :value-type ,value-type
         :documentation ,documentation
         :binding-table-var ,table-symbol
         :documentation-table-var ,doc-table-symbol))))

(defun lisp-namespace:clear-namespace (name)
  (clear-namespace name))

(setf
 ;; Package documentation
 (documentation (find-package '#:lisp-namespace) 't)
 "Utilities for defining additional namespaces in Common Lisp.
\
Common Lisp is a Lisp-N, which means that it has a different namespaces for
variables, functions, types, and so on. Users can also define their own
namespaces, and LISP-NAMESPACE is a toolkit for making that process easier.
\
Implemented via In Nomine."

 ;; Namespace definition and management
 (documentation 'define-namespace 'function)
 "Defines a new namespace object in the global namespace namespace along with
a series of functions, types, conditions, and type proclamations for accessing
this namespace.
\
The following syntax is supported:
* (DEFINE-NAMESPACE NAME &OPTIONAL VALUE-TYPE BINDING DOCUMENTATION)
  * NAME - a symbol naming the namespace,
  * VALUE-TYPE - a type specifier for values bound in this namespace,
  * BINDING - deprecated; must be NIL when provided,
  * DOCUMENTATION - documentation string for the namespace object.
* For name FOO, the following are generated:
  * Accessor functions SYMBOL-FOO and (SETF SYMBOL-FOO),
  * Makunbound function FOO-MAKUNBOUND,
  * Boundp function FOO-BOUNDP,
  * Type proclamations for the four functions above,
  * Condition type UNBOUND-FOO,
  * Type FOO-TYPE denoting the specified VALUE-TYPE,
  * Documentation methods with documentation type specialized on (EQL 'FOO),
  * Variable *FOO-TABLE* whose value is the binding table,
  * Variable *FOO-DOC-TABLE* whose value is the documentation table.
\
The consequences are undefined if a namespace is redefined in an incompatible
way with the previous one."

 (documentation 'clear-namespace 'function)
 "Removes all bindings in the namespace with the given name.")
