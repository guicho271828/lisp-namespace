;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

(defun check-long-form-arglist (arglist)
  (let ((length (list-length arglist)))
    (unless (and (numberp length) (evenp length))
      (error "Malformed property list in DEFINE-NAMESPACE.")))
  (loop with visited = '()
        for (keyword value) on arglist by #'cddr
        do (check-type keyword keyword)
        when (eq keyword :allow-other-keys)
          do (error "~S present in DEFINE-NAMESPACE." keyword)
        if (member keyword visited)
          do (error "Duplicate keyword ~S in DEFINE-NAMESPACE." keyword)
        else
          do (push keyword visited)))

(defun parse-long-form-arglist (args)
  (check-long-form-arglist args)
  (destructuring-bind
      (&key
         (accessor nil accessorp)
         (condition-name nil condition-name-p)
         (type-name nil type-name-p)
         (makunbound-symbol nil makunbound-symbol-p)
         (boundp-symbol nil boundp-symbol-p)
         (name-type 'symbol)
         (value-type 't)
         (test #'eql)
         (error-when-not-found-p t error-when-not-found-pp)
         (errorp-arg-in-accessor-p nil errorp-arg-in-accessor-pp)
         (default-arg-in-accessor-p t default-arg-in-accessor-pp)
         documentation)
      args
    (let ((arglist '()))
      (flet ((c (&rest args) (appendf arglist args)))
        (when accessorp (c :accessor accessor))
        (when condition-name-p (c :condition-name condition-name))
        (when type-name-p (c :type-name type-name))
        (when makunbound-symbol-p (c :makunbound-symbol makunbound-symbol))
        (when boundp-symbol-p (c :boundp-symbol boundp-symbol))
        (when error-when-not-found-pp
          (c :error-when-not-found-p error-when-not-found-p))
        (when errorp-arg-in-accessor-pp
          (c :errorp-arg-in-accessor-p errorp-arg-in-accessor-p))
        (when default-arg-in-accessor-pp
          (c :default-arg-in-accessor-p default-arg-in-accessor-p))
        (c :value-type value-type)
        (c :name-type name-type)
        (c :hash-table-test test))
      (values arglist documentation))))

(defun %define-namespace-long-form (name &rest args)
  (multiple-value-bind (arglist documentation) (parse-long-form-arglist args)
    (let ((namespace (apply #'ensure-namespace name arglist)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name (namespace-binding-table *namespaces*))
               (ensure-namespace ',name ,@args))
         ,@(make-proclamations namespace)
         ,@(make-unbound-condition-forms namespace)
         ,@(make-type-forms namespace)
         ,@(make-reader-forms namespace)
         ,@(make-writer-forms namespace)
         ,@(make-boundp-forms namespace)
         ,@(make-makunbound-forms namespace)
         ,@(make-documentation-forms namespace documentation)
         (symbol-namespace ',name)))))
