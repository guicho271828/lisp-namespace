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
   ;; TODO: get rid of NAMESPACE-LET and NSLET since they're just
   ;;       way too buggy and quite possibly unused
   ;; #:namespace-let
   ;; #:nslet
   ;; Namespace structure accessors
   #:namespace
   #:namespace-name
   #:namespace-name-type
   #:namespace-value-type
   #:namespace-accessor
   #:namespace-condition-name
   #:namespace-type-name
   ;; TODO get rid of NAMESPACE-LET-NAME along with the above
   ;; #:namespace-let-name
   #:namespace-makunbound-symbol
   #:namespace-boundp-symbol
   #:namespace-hash-table
   #:namespace-documentation-table
   ;; Namespace accessors
   #:symbol-namespace
   #:unbound-namespace
   #:namespace-type
   #:namespace-makunbound
   #:namespace-boundp))
