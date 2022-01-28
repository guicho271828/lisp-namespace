;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)
;;;;
;;;; KLUDGE: To our best knowledge, no Lisp implementation widely used in 2022
;;;;         defines a DESCRIBE-OBJECT that would be overwritten by our
;;;;         (DEFMETHOD DESCRIBE-OBJECT :AFTER ((OBJECT SYMBOL) STREAM) ...)
;;;;         This allows us to hook into each implementation's DESCRIBE-OBJECT
;;;;         in order to describe information about custom namespaces.
;;;;         This is a terrible hack relying on undefined behavior and the most
;;;;         scary thing about it is that it seems to work.

(in-package #:lisp-namespace)

(defun make-namespace-describer (symbol stream)
  (flet ((describe-symbol-in-namespace (namespace-name namespace)
           (when-let ((boundp (funcall (namespace-boundp-symbol namespace)
                                       symbol)))
             (pprint-logical-block (stream nil)
               (format stream "~@:_Symbol ~S is bound in a namespace ~S:"
                       symbol (namespace-name namespace))
               (pprint-indent :block 2 stream)
               (format stream "~@:_Value: ~S"
                       (funcall (namespace-accessor namespace) symbol))
               (if-let ((doc (documentation symbol namespace-name)))
                 (progn
                   (format stream "~@:_Documentation: ~@:_")
                   (pprint-logical-block (stream nil :per-line-prefix "  ")
                     (princ doc stream)))
                 (format stream "~@:_(undocumented)"))))))
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
      (let* ((specializers (list (find-class 'symbol) (find-class 't)))
             (qualifiers '(:after))
             (method (find-method #'describe-object
                                  qualifiers specializers nil)))
        (if (or (null method)
                (eq method *describe-object-method*))
            (let ((new-method ,@body))
              (setf *describe-object-method* new-method)
              new-method)
            (warn "A previous DESCRIBE-OBJECT :AFTER (SYMBOL T) method which ~
                   was not defined by LISP-NAMESPACE already exists; ~
                   LISP-NAMESPACE will NOT overwrite it with a custom ~
                   method."))))))

(with-describe-object-method-handling
  (defmethod describe-object :after ((symbol symbol) stream)
    (let ((*print-pretty* t)
          (describer (make-namespace-describer symbol stream)))
      (pprint-logical-block (stream nil)
        (maphash describer (namespace-hash-table *namespaces*))))))
