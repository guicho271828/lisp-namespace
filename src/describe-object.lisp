;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)
;;;;
;;;; KLUDGE: To our best knowledge, no Lisp implementation widely used in 2022
;;;;         defines a DESCRIBE-OBJECT that would be overwritten by our
;;;;         (DEFMETHOD DESCRIBE-OBJECT :AFTER (OBJECT STREAM) ...)
;;;;         This allows us to hook into each implementation's DESCRIBE-OBJECT
;;;;         in order to describe information about custom namespaces.
;;;;         This is a terrible hack relying on undefined behavior and the most
;;;;         scary thing about it is that it seems to work.

(in-package #:in-nomine)

(defun make-namespace-describer (thing stream)
  (flet ((describe-symbol-in-namespace (namespace-name namespace)
           (when (typep thing (namespace-name-type namespace))
             (when-let* ((boundp-symbol (namespace-boundp-symbol namespace))
                         (boundp (funcall boundp-symbol thing)))
               (pprint-logical-block (stream nil)
                 (format stream "~@:_~S is bound in namespace ~S:"
                         thing (namespace-name namespace))
                 (pprint-indent :block 2 stream)
                 (when-let ((accessor (namespace-accessor namespace)))
                   (format stream "~@:_Value: ~S" (funcall accessor thing)))
                 (if-let ((doc (documentation thing namespace-name)))
                   (progn
                     (format stream "~@:_Documentation: ~@:_")
                     (pprint-logical-block (stream nil :per-line-prefix "  ")
                       (princ doc stream)))
                   (format stream "~@:_(undocumented)")))))))
    #'describe-symbol-in-namespace))

(defvar *describe-object-method* nil)

(defmacro with-describe-object-method-handling (&body body)
  (let ((prologue '(;; CLISP - package lock
                    #+clisp ext:without-package-lock
                    #+clisp ("COMMON-LISP")
                    ;; LispWorks - redefinition warning
                    #+lispworks let
                    #+lispworks ((lispworks:*handle-warn-on-redefinition* nil))
                    ;; SBCL - no warning on 2.2.0, but let's be safe
                    #+sbcl sb-ext:without-package-locks
                    ;; Other implementations - no warnings
                    #-(or clisp lispworks sbcl) progn)))
    `(,@prologue
      (let* ((specializers (list (find-class 't) (find-class 't)))
             (qualifiers '(:after))
             (method (find-method #'describe-object
                                  qualifiers specializers nil)))
        (if (or (null method)
                (eq method *describe-object-method*))
            (let ((new-method ,@body))
              (setf *describe-object-method* new-method)
              new-method)
            (warn "A previous DESCRIBE-OBJECT :AFTER (T T) method which ~
                   was not defined by IN-NOMINE already exists; ~
                   IN-NOMINE will NOT overwrite it with a custom ~
                   method."))))))

(with-describe-object-method-handling
  (defmethod describe-object :after (thing stream)
    (let ((*print-pretty* t)
          (describer (make-namespace-describer thing stream)))
      (pprint-logical-block (stream nil)
        (maphash describer (namespace-binding-table *namespaces*))))))
