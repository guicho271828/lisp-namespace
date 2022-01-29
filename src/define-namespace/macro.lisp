;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:lisp-namespace)

(defmacro define-namespace (name &rest args)
  (if (and (not (null args)) (keywordp (first args)))
      (apply #'%define-namespace-long-form name args)
      (apply #'%define-namespace-short-form name args)))

(define-namespace namespace namespace nil)

(defun clear-namespace (name)
  (when (eq name 'namespace)
    (error "Clearing the namespace of all namespaces is a bad idea."))
  (let ((namespace (symbol-namespace name)))
    (clrhash (namespace-binding-table namespace))
    (clrhash (namespace-documentation-table namespace))
    name))
