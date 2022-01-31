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
         (name-type nil name-type-p)
         (value-type nil value-type-p)
         (accessor nil accessorp)
         (condition-name nil condition-name-p)
         (type-name nil type-name-p)
         (makunbound-symbol nil makunbound-symbol-p)
         (boundp-symbol nil boundp-symbol-p)
         (documentation-type nil documentation-type-p)
         (hash-table-test nil hash-table-test-p)
         (error-when-not-found-p nil error-when-not-found-pp)
         (errorp-arg-in-accessor-p nil errorp-arg-in-accessor-pp)
         (default-arg-in-accessor-p nil default-arg-in-accessor-pp)
         (binding-table-var nil binding-table-var-p)
         (documentation-table-var nil documentation-table-var-p)
         documentation)
      args
    (let ((arglist '()))
      (flet ((c (&rest args) (appendf arglist args)))
        (when name-type-p (c :name-type name-type))
        (when value-type-p (c :value-type value-type))
        (when accessorp (c :accessor accessor))
        (when condition-name-p (c :condition-name condition-name))
        (when type-name-p (c :type-name type-name))
        (when makunbound-symbol-p (c :makunbound-symbol makunbound-symbol))
        (when boundp-symbol-p (c :boundp-symbol boundp-symbol))
        (when documentation-type-p
          (c :documentation-type documentation-type))
        (when error-when-not-found-pp
          (c :error-when-not-found-p error-when-not-found-p))
        (when errorp-arg-in-accessor-pp
          (c :errorp-arg-in-accessor-p errorp-arg-in-accessor-p))
        (when default-arg-in-accessor-pp
          (c :default-arg-in-accessor-p default-arg-in-accessor-p))
        (when hash-table-test-p (c :hash-table-test hash-table-test))
        (when binding-table-var-p
          (c :binding-table-var binding-table-var))
        (when documentation-table-var-p
          (c :documentation-table-var documentation-table-var)))
      (values arglist documentation))))

(defun %define-namespace-long-form (name &rest args)
  (multiple-value-bind (arglist documentation) (parse-long-form-arglist args)
    (let ((namespace (apply #'ensure-namespace name arglist)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name (namespace-binding-table *namespaces*))
               (apply #'ensure-namespace ',name ',args))
         ,@(make-proclamations namespace)
         ,@(make-unbound-condition-forms namespace)
         ,@(make-type-forms namespace)
         ,@(make-reader-forms namespace)
         ,@(make-writer-forms namespace)
         ,@(make-boundp-forms namespace)
         ,@(make-makunbound-forms namespace)
         ,@(make-documentation-forms namespace documentation)
         ,@(make-binding-table-var-forms namespace)
         ,@(make-documentation-table-var-forms namespace)
         (symbol-namespace ',name)))))
