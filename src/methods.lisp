;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

;;; Methods on standard functions

(defmethod documentation ((namespace namespace) (type (eql 't)))
  (namespace-documentation namespace))

(defmethod (setf documentation) (newdoc (namespace namespace) (type (eql 't)))
  (setf (namespace-documentation namespace) newdoc))

(defmethod documentation ((name symbol) (type (eql 'namespace)))
  (namespace-documentation (symbol-namespace name)))

(defmethod (setf documentation) (newdoc (name symbol) (type (eql 'namespace)))
  (setf (namespace-documentation (symbol-namespace name)) newdoc))

(defmethod print-object ((namespace namespace) stream)
  (print-unreadable-object (namespace stream :type t)
    (let ((hash-table (namespace-binding-table namespace)))
      (format stream "~S ~:[(external)~;(~D binding~:*~P)~]"
              (namespace-name namespace)
              hash-table
              (when hash-table
                (hash-table-count (namespace-binding-table namespace)))))))
