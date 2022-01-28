;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:lisp-namespace)

(defun make-proclamations (namespace)
  (let ((name-type (namespace-name-type namespace))
        (accessor (namespace-accessor namespace))
        (boundp (namespace-boundp-symbol namespace))
        (makunbound (namespace-makunbound-symbol namespace))
        (type (namespace-type-name namespace)))
    `(,@(when accessor
          `((declaim (ftype (function (,name-type &optional (or null ,type))
                                      (values ,type &optional))
                            ,accessor)
                     (inline ,accessor))
            (declaim (ftype (function (,type ,name-type)
                                      (values ,type &optional))
                            (setf ,accessor))
                     (inline (setf ,accessor)))))
      ,@(when boundp
          `((declaim (ftype (function (,name-type) (values boolean &optional))
                            ,boundp))))
      ,@(when makunbound
          `((declaim (ftype (function (,name-type)
                                      (values ,name-type &optional))
                            ,makunbound)))))))

(defun make-unbound-condition-forms (namespace)
  (let ((name (namespace-name namespace))
        (condition (namespace-condition-name namespace)))
    (when condition
      `((define-condition ,condition (cell-error) ()
          (:report (lambda (condition stream)
                     (format stream "Name ~S is unbound in namespace ~S."
                             (cell-error-name condition) ',name))))))))

(defun make-type-forms (namespace)
  (let ((type-name (namespace-type-name namespace))
        (value-type (namespace-value-type namespace)))
    (when type-name
      `((deftype ,type-name () ',value-type)))))

(defun read-evaluated-form ()
  (format *query-io* "~&;; Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defun make-reader-forms (namespace)
  (let ((name (namespace-name namespace))
        (accessor (namespace-accessor namespace))
        (condition (namespace-condition-name namespace)))
    `((defun ,accessor (name &optional (default nil defaultp))
        ,(format nil "Automatically defined reader function.~%~
                      When DEFAULT is supplied, the value is set automatically ~
                      if the symbol is not bound in the namespace.")
        ;; We need special treatment for namespace NAMESPACE in order to break
        ;; the metacycle in #'SYMBOL-NAMESPACE.
        (let* ((namespace ,(if (eq name 'namespace)
                               '*namespaces*
                               `(symbol-namespace ',name)))
               (hash-table (namespace-hash-table namespace)))
          (multiple-value-bind (value foundp) (gethash name hash-table)
            (cond (foundp value)
                  (defaultp (setf (gethash name hash-table) default))
                  (t (restart-case (error ',condition :name name)
                       (use-value (newval)
                         :report "Use specified value."
                         :interactive read-evaluated-form
                         newval)
                       (store-value (newval)
                         :report "Set specified value and use it."
                         :interactive read-evaluated-form
                         (setf (gethash name hash-table) newval)))))))))))

(defun make-writer-forms (namespace)
  (let ((name (namespace-name namespace))
        (accessor (namespace-accessor namespace)))
    `((defun (setf ,accessor) (new-value name)
        "Automatically defined writer function."
        (let* ((namespace (symbol-namespace ',name))
               (hash-table (namespace-hash-table namespace)))
          (setf (gethash name hash-table) new-value))))))

(defun make-boundp-forms (namespace)
  (let ((name (namespace-name namespace))
        (boundp (namespace-boundp-symbol namespace)))
    `((defun ,boundp (name)
        "Automatically defined boundp function."
        (let* ((namespace (symbol-namespace ',name))
               (hash-table (namespace-hash-table namespace)))
          (nth-value 1 (gethash name hash-table)))))))

(defun make-makunbound-forms (namespace)
  (let ((name (namespace-name namespace))
        (makunbound (namespace-makunbound-symbol namespace)))
    `((defun ,makunbound (name)
        "Automatically defined makunbound function."
        (let* ((namespace (symbol-namespace ',name))
               (hash-table (namespace-hash-table namespace)))
          (remhash name hash-table)
          name)))))

(defun make-documentation-forms (namespace documentation)
  (let ((name (namespace-name namespace)))
    `((defmethod documentation (name (type (eql ',name)))
        (let ((namespace (symbol-namespace ',name)))
          (gethash name (namespace-documentation-table namespace))))
      (defmethod (setf documentation) (newdoc name (type (eql ',name)))
        (let* ((namespace (symbol-namespace ',name))
               (doc-table (namespace-documentation-table namespace)))
          (if (null newdoc)
              (remhash name doc-table)
              (setf (gethash name doc-table) newdoc))))
      ,@(when documentation
          `((setf (documentation ',name 'namespace) ,documentation))))))
