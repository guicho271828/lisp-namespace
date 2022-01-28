;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:lisp-namespace)

(defun check-long-form-plist (plist)
  (let ((length (list-length plist)))
    (unless (and (numberp length) (evenp length))
      (error "Malformed property list in DEFINE-NAMESPACE.")))
  (loop with visited = '()
        for (keyword value) on plist by #'cddr
        do (check-type keyword keyword)
        when (eq keyword :allow-other-keys)
          do (error "~S present in DEFINE-NAMESPACE." keyword)
        if (member keyword visited)
          do (error "Duplicate keyword ~S in DEFINE-NAMESPACE." keyword)
        else
          do (push keyword visited)))

(defun %define-namespace-long-form (name &rest args)
  (check-long-form-plist args)
  (destructuring-bind (&key
                         (accessor nil accessorp)
                         (condition-name nil condition-name-p)
                         (type-name nil type-name-p)
                         (makunbound-symbol nil makunbound-symbol-p)
                         (boundp-symbol nil boundp-symbol-p)
                         (name-type 'symbol)
                         (type 't)
                         (test #'eql)
                         documentation)
      args
    (let ((arglist '()))
      (flet ((c (&rest args) (appendf arglist args)))
        (when accessorp (c :accessor accessor))
        (when condition-name-p (c :condition-name condition-name))
        (when type-name-p (c :type-name type-name))
        (when makunbound-symbol-p (c :makunbound-symbol makunbound-symbol))
        (when boundp-symbol-p (c :boundp-symbol boundp-symbol))
        (c :name-type name-type)
        (c :hash-table-test test))
      (let ((namespace (apply #'ensure-namespace name arglist)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (gethash ',name (namespace-hash-table *namespaces*))
                 (ensure-namespace ',name ,@args))
           ))))
  (error "Not implemented yet."))
