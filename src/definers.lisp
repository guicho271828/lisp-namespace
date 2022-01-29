;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:in-nomine)

;;; Minor forms

(defun make-proclamations (namespace)
  (let* ((name (namespace-name namespace))
         (name-type (namespace-name-type namespace))
         (accessor (namespace-accessor namespace))
         (boundp (namespace-boundp-symbol namespace))
         (makunbound (namespace-makunbound-symbol namespace))
         (type (or (namespace-type-name namespace) 't))
         (errorp-arg-p (namespace-errorp-arg-in-accessor-p namespace))
         (default-arg-p (namespace-default-arg-in-accessor-p namespace))
         (error-when-not-found-p (namespace-error-when-not-found-p namespace)))
    `((declaim
       ,@(when accessor
           `((ftype (function (,name-type
                               &optional
                               ,@(when errorp-arg-p `(t))
                               ,@(when default-arg-p `((or ,type null))))
                              (values ,(if (and error-when-not-found-p
                                                (not errorp-arg-p))
                                           type
                                           `(or ,type null))
                                      &optional))
                    ,accessor)
             (inline ,accessor)))
       ;; We do not generate a writer for NAMESPACE.
       ;; TODO separate reader and writer names in namespace definition
       ,@(when (and accessor (not (eq name 'namespace)))
           `((ftype (function (,type ,name-type &optional
                                     ,@(when errorp-arg-p `(t))
                                     ,@(when default-arg-p
                                         `((or ,type null))))
                              (values ,type &optional))
                    (setf ,accessor))
             (inline (setf ,accessor))))
       ,@(when boundp
           `((ftype (function (,name-type) (values boolean &optional)) ,boundp)))
       ,@(when makunbound
           `((ftype (function (,name-type)
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

(defun make-boundp-forms (namespace)
  (let ((name (namespace-name namespace))
        (boundp (namespace-boundp-symbol namespace)))
    (when boundp
      `((defun ,boundp (name)
          "Automatically defined boundp function."
          (let* ((namespace (symbol-namespace ',name))
                 (hash-table (namespace-binding-table namespace)))
            (nth-value 1 (gethash name hash-table))))))))

(defun make-makunbound-forms (namespace)
  (let ((name (namespace-name namespace))
        (makunbound (namespace-makunbound-symbol namespace)))
    (when makunbound
      `((defun ,makunbound (name)
          "Automatically defined makunbound function."
          ,@(if (eq name 'namespace)
                `((error "Unable to remove the NAMESPACE namespace."))
                `((let* ((namespace (symbol-namespace ',name))
                         (hash-table (namespace-binding-table namespace)))
                    (remhash name hash-table)
                    name))))))))

(defun make-documentation-forms (namespace documentation)
  (let ((name (namespace-name namespace))
        (documentation-type (namespace-documentation-type namespace)))
    `(,@(when documentation-type
          `((defmethod documentation (name (type (eql ',documentation-type)))
              (let ((namespace (symbol-namespace ',name)))
                (values (gethash name
                                 (namespace-documentation-table namespace)))))
            (defmethod (setf documentation)
                (newdoc name (type (eql ',documentation-type)))
              (let* ((namespace (symbol-namespace ',name))
                     (doc-table (namespace-documentation-table namespace)))
                (if (null newdoc)
                    (remhash name doc-table)
                    (setf (gethash name doc-table) newdoc))))))
      ,@(when documentation
          `((setf (documentation ',name 'namespace) ,documentation))))))

;;; Reader forms

(defun read-evaluated-form ()
  (format *query-io* "~&;; Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defun make-reader-forms (namespace)
  (let ((name (namespace-name namespace))
        (accessor (namespace-accessor namespace))
        (condition (namespace-condition-name namespace))
        (default-errorp (namespace-error-when-not-found-p namespace))
        (errorp-arg-p (namespace-errorp-arg-in-accessor-p namespace))
        (default-arg-p (namespace-default-arg-in-accessor-p namespace)))
    (when accessor
      `((defun ,accessor
            (name &optional
                    ,@(when errorp-arg-p `((errorp ,default-errorp errorpp)))
                    ,@(when default-arg-p `((default nil defaultp))))
          ,@(when errorp-arg-p `((declare (ignorable errorp errorpp))))
          ,@(when default-arg-p `((declare (ignorable default defaultp))))
          ,(format nil
                   "Automatically defined reader function.~%~
                    ~:[Returns NIL~;Signals ~:*~S~] if the value is not found ~
                    in the namespace~:[~;, unless ERRORP is set to false~].~
                    ~:[~;~%When DEFAULT is supplied and the symbol is not ~
                    bound, the default value is automatically set.~]"
                   condition errorp-arg-p default-arg-p)
          ;; We need special treatment for namespace NAMESPACE in order to break
          ;; the metacycle in #'SYMBOL-NAMESPACE.
          (let* ((namespace ,(if (eq name 'namespace)
                                 '*namespaces*
                                  `(symbol-namespace ',name)))
                 (hash-table (namespace-binding-table namespace)))
            (multiple-value-bind (value foundp) (gethash name hash-table)
              (cond (foundp value)
                    ,@(when default-arg-p
                        `((defaultp (setf (gethash name hash-table) default))))
                    ,@(when (and condition (or default-errorp errorp-arg-p))
                        `((,(cond (errorp-arg-p 'errorp)
                                  (default-errorp 't))
                           (restart-case (error ',condition :name name)
                             (use-value (newval)
                               :report "Use specified value."
                               :interactive read-evaluated-form
                               newval)
                             (store-value (newval)
                               :report "Set specified value and use it."
                               :interactive read-evaluated-form
                               (setf (gethash name hash-table)
                                     newval))))))))))))))

;;; Writer foms

(defun make-writer-forms (namespace)
  (let ((name (namespace-name namespace))
        (accessor (namespace-accessor namespace))
        (errorp-arg-p (namespace-errorp-arg-in-accessor-p namespace))
        (default-arg-p (namespace-default-arg-in-accessor-p namespace)))
    (when (and accessor (not (eq name 'namespace)))
      `((defun (setf ,accessor)
            (new-value name &optional
                              ,@(when errorp-arg-p `((errorp nil)))
                              ,@(when default-arg-p `((default nil))))
          "Automatically defined writer function."
          ,@(when errorp-arg-p `((declare (ignore errorp))))
          ,@(when default-arg-p `((declare (ignore default))))
          (let* ((namespace (symbol-namespace ',name))
                 (hash-table (namespace-binding-table namespace)))
            (setf (gethash name hash-table) new-value)))))))
