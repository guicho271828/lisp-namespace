;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

(defmacro define-namespace (name &rest args)
  (if (and (not (null args)) (keywordp (first args)))
      (apply #'%define-namespace-long-form name args)
      (apply #'%define-namespace-short-form name args)))

(macrolet ((define ()
             `(define-namespace namespace
                ,@*namespace-args*)))
  (define))

(defun clear-namespace (name)
  (when (eq name 'namespace)
    (cerror "Proceed." "Attempting to clear the namespace of all namespaces."))
  (let* ((namespace (symbol-namespace name))
         (binding-table (namespace-binding-table namespace))
         (documentation-table (namespace-documentation-table namespace)))
    (when binding-table (clrhash binding-table))
    (when documentation-table (clrhash documentation-table))
    (when (eq name 'namespace)
      (setf (gethash 'namespace (namespace-binding-table namespace)) namespace))
    name))
