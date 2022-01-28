;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:lisp-namespace)

(defun check-name-not-in-cl-package (name)
  (when (eq (symbol-package name) (find-package :common-lisp))
    (error "~S is a standard Common Lisp symbol and it cannot be used ~
            as a namespace name." name)))

(defun check-redefine-meta-namespace (name)
  (when (eq name 'namespace)
    (unless (eq *package* (find-package '#:lisp-namespace))
      (cerror "Redefine the namespace."
              "Attempting to redefine namespace NAMESPACE."))))
