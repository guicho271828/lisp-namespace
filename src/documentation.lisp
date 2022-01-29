;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

(setf
 (documentation (find-package :in-nomine) 't)
 "Utilities for defining additional namespaces in Common Lisp.

Common Lisp is a Lisp-N, which means that it has a different namespaces for
variables, functions, types, and so on. Users can also define their own
namespaces, and IN-NOMINE is a toolkit for making that process easier."
 (documentation 'namespace 'type)
 "A namespace object, representing a Common Lisp namespace."
 (documentation 'define-namespace 'function)
 ;; TODO implement and document long form
 "This macro defines a namespace. For the given name of namespace X,
DEFINE-NAMESPACE defines 4 functions/macros:

+ #'SYMBOL-X, #'(setf SYMBOL-X) : accessor to the global binding. Optionally,
  EXPECTED-TYPE provides FTYPE proclamation and results in the
  better optimization. EXPECTED-TYPE is not evaluated.
+ #'X-BOUNDP : unary function returning a boolean
+ condition UNBOUND-X which is signaled when trying to access the value of an
  unbound symbol.
+ macro (X-LET (binding...) body) : lexical binding. It is defined when BINDING
  is non-nil. "
 (documentation 'clear-namespace 'function)
 "Get rid of all values bound in the given namespace."
 (documentation 'namespace 'namespace)
 "A namespace for managing namespaces.")

;; TODO all exports
