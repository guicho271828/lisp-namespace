;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(uiop:define-package #:in-nomine
  (:use #:cl #:alexandria)
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
   #:namespace-binding-table-var
   #:namespace-documentation-table-var
   ;; Metanamespace accessors
   #:namespace
   #:symbol-namespace
   #:namespace-boundp
   #:namespace-makunbound
   #:unbound-namespace))
