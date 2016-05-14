
#|

This file provide additional namespace for lisp.

Common lisp is lisp-2, which means it has a different namespaces for the
value and the function. With lisp-n, you can define arbitrary additional
namespaces and its accessors as well.

The idea is simple.  Common lisp has `symbol-value' and `symbol-function',
so I added `symbol-anything-you-like'.  Current implementation is
built upon a hashtable, but it also modifies `cl:symbol-plist', for the
debugging purpose. I assume there won't be so many additional namespaces.

|#

(defpackage :lisp-namespace
  (:use :cl :alexandria)
  (:nicknames :lispn)
  (:export :define-namespace
           :clear-namespace
           :namespace-let
           :nslet))

(in-package :lispn)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; the name of this variable shoud not be changed, to maintain consistency
  ;; to the hash tables defined by define-namespace.
  (defvar *namespace-hash* (make-hash-table :test 'eq))
  (defstruct (%namespace
              (:constructor %namespace
                            (name
                             &aux
                             (accessor  (symbolicate "SYMBOL-" name))
                             (hash      (symbolicate "*" name "-TABLE*"))
                             (condition (symbolicate "UNBOUND-" name))
                             (boundp    (symbolicate name "-BOUNDP"))
                             (type      (symbolicate name "-TYPE"))
                             (letname   (symbolicate name "-LET"))
                             (doc-table (symbolicate "*" name "-DOC-TABLE*")))))
    (name      (error "anonymous namespace?")   :type symbol :read-only t)
    (accessor  nil :type symbol :read-only t)
    (hash      nil :type symbol :read-only t) ;; default values are fed by the constructor above
    (condition nil :type symbol :read-only t)
    (boundp    nil :type symbol :read-only t)
    (type      nil :type symbol :read-only t)
    (letname   nil :type symbol :read-only t)
    (doc-table nil :type symbol :read-only t))
  (defmethod make-load-form ((ns %namespace) &optional environment)
    (make-load-form-saving-slots ns :environment environment)))


(defmacro define-namespace (name &optional
                                   (expected-type t)
                                   (namespace-let t)
                                   (documentation ""))
  (when (member name '(function
                       macrolet
                       name
                       package
                       plist
                       value))
    (error "~a cannot be used as a namespace because it conflicts with the standard Common Lisp!"
           name))
  (let ((ns (%namespace name)))
    (with-slots (accessor hash condition boundp letname type doc-table) ns
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          (defvar ,hash (make-hash-table :test 'eq))
          (defvar ,doc-table (make-hash-table :test 'eq))
          (define-condition ,condition (unbound-variable) ()
            (:report (lambda (c s) (format s "Symbol ~a is unbound in namespace ~a"
                                           (cell-error-name c) ',name))))
          (deftype ,type () ',expected-type)
          (declaim (ftype (function (symbol &optional ,type) (,type)) ,accessor)
                   (ftype (function ((,type) symbol) (,type)) (setf ,accessor))
                   (inline ,accessor)
                   (inline (setf ,accessor)))
          (defun (setf ,accessor) (new-value symbol)
            "Automatically defined setter function."
            (setf (gethash symbol ,hash) new-value))
          (defun ,accessor (symbol &optional (default nil default-supplied-p))
            "Automatically defined getter function. When DEFAULT is supplied, the value is set automatically."
            (multiple-value-bind (value found)
                (gethash symbol ,hash)
              (if found value
                  (if default-supplied-p
                      (setf (,accessor symbol) default)
                      (restart-case
                          (error ',condition :name symbol)
                        (use-value (default)
                          (setf (,accessor symbol) default)))))))
          (defun ,boundp (symbol)
            "Automatically defined boolean function."
            (nth-value 1 (gethash symbol ,hash)))
          ,@(when namespace-let
              `((defmacro ,letname (bindings &body body)
                  `(namespace-let
                       ,(mapcar (lambda (bind) `((,',name ,(car bind)) ,@(cdr bind)))
                                bindings)
                     ,@body))))
          (setf (gethash ',name *namespace-table*) ,ns)
          (defmethod documentation ((x symbol) (type (eql ',name)))
            (gethash x ,doc-table))
          (defmethod (setf documentation) (newdoc (x symbol) (type (eql ',name)))
            (setf (gethash x ,doc-table) newdoc))
          (setf (documentation ',name 'namespace) ,documentation)))))

(define-namespace namespace %namespace nil "A namespace for managing namespaces themselves.")

(defun clear-namespace (name)
  (assert (gethash name *namespace-table*))
  (clrhash (symbol-value (%namespace-hash (gethash name *namespace-table*))))
  name)

(defmethod describe-object :after ((x symbol) s)
  (pprint-logical-block (s nil)
    (maphash (lambda (name ns)
               (when (funcall (%namespace-boundp ns) x)
                 (pprint-logical-block (s nil)
                   (format s "~@:_Symbol ~S is bound in a namespace ~S:" x name)
                   (pprint-indent :block 2 s)
                   (format s "~@:_Value: ~S" (funcall (%namespace-accessor ns) x))
                   (if-let ((doc (documentation x name)))
                     (progn
                       (format s "~@:_Documentation: ~@:_")
                       (pprint-logical-block (s nil :per-line-prefix "  ")
                         (princ doc s)))
                     (format s "~@:_(undocumented)")))))
             *namespace-table*)))

;; (princ (documentation 'namespace 'namespace))

