;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:lisp-namespace)

(deftype unbound-behavior () '(member :error :errorp))

(defstruct (namespace (:constructor %make-namespace))
  (name                      nil :type symbol  :read-only t)
  (name-type                 nil :type t       :read-only t)
  (value-type                nil :type t       :read-only t)
  (accessor                  nil :type symbol  :read-only t)
  (condition-name            nil :type symbol  :read-only t)
  (type-name                 nil :type symbol  :read-only t)
  (makunbound-symbol         nil :type symbol  :read-only t)
  (boundp-symbol             nil :type symbol  :read-only t)
  ;; TODO use these three in the definers and the long form
  (error-when-not-found-p    t   :type boolean :read-only t)
  (errorp-arg-in-accessor-p  nil :type boolean :read-only t)
  (default-arg-in-accessor-p t   :type boolean :read-only t)
  (hash-table                (make-hash-table :test #'eq) :type hash-table)
  (documentation-table       (make-hash-table :test #'eq) :type hash-table))

(defun make-namespace
    (name &key
            (accessor (symbolicate '#:symbol- name))
            (condition-name (symbolicate '#:unbound- name))
            (type-name (symbolicate name '#:-type))
            (makunbound-symbol (symbolicate name '#:-makunbound))
            (boundp-symbol (symbolicate name '#:-boundp))
            (hash-table-test #'eq)
            (name-type 'symbol)
            (value-type 't)
            (error-when-not-found-p t)
            (errorp-arg-in-accessor-p nil)
            (default-arg-in-accessor-p t))
  (%make-namespace
   :name name :name-type name-type :value-type value-type
   :accessor accessor
   :condition-name condition-name :type-name type-name
   :makunbound-symbol makunbound-symbol :boundp-symbol boundp-symbol
   :hash-table (make-hash-table :test hash-table-test)
   :documentation-table (make-hash-table :test hash-table-test)
   :error-when-not-found-p error-when-not-found-p
   :errorp-arg-in-accessor-p errorp-arg-in-accessor-p
   :default-arg-in-accessor-p default-arg-in-accessor-p))

(defmethod print-object ((namespace namespace) stream)
  (print-unreadable-object (namespace stream :type t)
    (format stream "~S (~D binding~:*~P)"
            (namespace-name namespace)
            (hash-table-count (namespace-hash-table namespace)))))

(defvar *namespaces* (make-namespace 'namespace :value-type 'namespace))

(defun ensure-namespace (name &rest args)
  (let ((hash-table (namespace-hash-table *namespaces*)))
    (multiple-value-bind (value foundp) (gethash name hash-table)
      (if foundp
          value
          (apply #'make-namespace name args)))))

(setf *namespaces* (ensure-namespace 'namespace :value-type 'namespace)
      (gethash 'namespace (namespace-hash-table *namespaces*)) *namespaces*)
