;;;; This file is a part of LISP-NAMESPACE
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(uiop:define-package #:lisp-namespace
  (:use #:cl #:alexandria)
  (:nicknames #:lispn)
  (:export #:define-namespace
           #:clear-namespace))

(in-package #:lisp-namespace)

(defmacro define-namespace (name &optional
                                   (value-type 't)
                                   (letp t letpp)
                                   documentation)
  (when (and letpp letp)
    (warn "Deprecated option BINDING used in DEFINE-NAMESPACE: ~
             no binding form was generated."))
  (let ((table-symbol (symbolicate '#:* name '#:-table*))
        (doc-table-symbol (symbolicate '#:* name '#:-doc-table*)))
    `(in-nomine:define-namespace ,name
       :value-type ,value-type
       :documentation ,documentation
       :makunbound-symbol nil
       :binding-table-var ,table-symbol
       :documentation-table-var ,doc-table-symbol)))

(defun clear-namespace (name)
  (in-nomine:clear-namespace name))

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
