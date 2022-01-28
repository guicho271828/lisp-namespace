;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:lisp-namespace)

;; TODO export these functions to some shared file

(defun check-name-not-in-cl-package (name)
  (when (eq (symbol-package name) (find-package :common-lisp))
    (error "~S is a standard Common Lisp symbol and it cannot be used ~
            as a namespace name." name)))

(defun check-redefine-meta-namespace (name)
  (when (eq name 'namespace)
    (unless (eq *package* (find-package '#:lisp-namespace))
      (cerror "Redefine the namespace."
              "Attempting to redefine namespace NAMESPACE."))))

(defun %define-namespace-short-form (name &optional
                                            (value-type 't)
                                            (letp t letpp)
                                            documentation)
  (when (and letpp letp)
    (warn "Deprecated option BINDING provided to DEFINE-NAMESPACE: ~
           no binding form was generated."))
  (check-name-not-in-cl-package name)
  (check-redefine-meta-namespace name)
  (let ((namespace (ensure-namespace name :value-type value-type)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name (namespace-hash-table *namespaces*))
             (ensure-namespace ',name :value-type ',value-type))
       ,@(make-proclamations namespace)
       ,@(make-unbound-condition-forms namespace)
       ,@(make-type-forms namespace)
       ,@(make-reader-forms namespace)
       ,@(make-writer-forms namespace)
       ,@(make-boundp-forms namespace)
       ,@(make-makunbound-forms namespace)
       ,@(make-documentation-forms namespace documentation)
       (symbol-namespace ',name))))

