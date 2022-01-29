;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(uiop:define-package #:lisp-namespace
  (:use #:cl #:alexandria)
  (:nicknames #:lispn)
  (:export
   ;; Macros and utility functions
   #:define-namespace
   #:clear-namespace
   ;; Namespace structure accessors
   #:namespace
   #:namespace-name
   #:namespace-name-type
   #:namespace-value-type
   #:namespace-accessor
   #:namespace-condition-name
   #:namespace-type-name
   #:namespace-makunbound-symbol
   #:namespace-boundp-symbol
   #:namespace-documentation-type
   #:namespace-error-when-not-found-p
   #:namespace-errorp-arg-in-accessor-p
   #:namespace-default-arg-in-accessor-p
   #:namespace-hash-table-test
   #:namespace-binding-table
   #:namespace-documentation-table
   ;; Namespace accessors
   #:symbol-namespace
   #:unbound-namespace
   #:namespace-type
   #:namespace-makunbound
   #:namespace-boundp))
