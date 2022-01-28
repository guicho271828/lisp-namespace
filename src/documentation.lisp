;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:lisp-namespace)

(setf
 (documentation (find-package :lisp-namespace) 't)
 "Utilities for defining additional namespaces in Common Lisp.

Common Lisp is a Lisp-N, which means that it has a different namespaces for
variables, functions, types, and so on. Users can also define their own
namespaces, and LISP-NAMESPACE is a toolkit for making that process easier."
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
 "A namespace for managing namespaces."
 ;; TODO: remove this altogether
 ;; (documentation 'namespace-let 'function)
 ;;  #1="Bindings is a list of bindings where each car is of form (NAMESPACE NAME),
 ;;  or a symbol NAME for a variable namespace.

 ;;  function, macro, label, symbol-macro, handler, restart is by default
 ;;  recognized as a namespace.

 ;; Example:
 ;; (namespace-let ((#'x (y) (1+ y)) ; -- equivalent to ((function x) (y) (1+ y))
 ;;                 ((macro x) (y) (1+ y))
 ;;                 ((macro y) (y) (1+ y))
 ;;                 (#'x (y) (1+ y))
 ;;                 ((label y) (y) (y y))
 ;;                 ((symbol-macro sm) 0)
 ;;                 (b 0))
 ;;   (let ((b 1))
 ;;     (print :x)))
 ;; "
 ;;  (documentation 'nslet 'function)
 ;;  #1#
 )

;; TODO all exports
