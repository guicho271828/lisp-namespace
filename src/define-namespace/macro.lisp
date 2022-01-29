;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

(defmacro define-namespace (name &rest args)
  (if (and (not (null args)) (keywordp (first args)))
      (apply #'%define-namespace-long-form name args)
      (apply #'%define-namespace-short-form name args)))

(define-namespace namespace
  :value-type namespace
  :documentation-type nil)

(defun clear-namespace (name)
  (when (eq name 'namespace)
    (error "Clearing the namespace of all namespaces is a bad idea."))
  (let ((namespace (symbol-namespace name)))
    (clrhash (namespace-binding-table namespace))
    (clrhash (namespace-documentation-table namespace))
    name))
