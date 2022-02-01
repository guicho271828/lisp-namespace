;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(defpackage #:lisp-namespace/test
  (:use #:cl
        #:lisp-namespace
        #:fiveam)
  (:export #:lisp-namespace))

(in-package #:lisp-namespace/test)

(def-suite lisp-namespace)
(in-suite lisp-namespace)

(define-namespace bazinga keyword nil
  "A bazinga namespace.")

(test lisp-namespace
  ;; Accessor
  (setf (symbol-bazinga 'this-bazinga) :bazinga)
  (is (eq :bazinga (symbol-bazinga 'this-bazinga)))
  ;; Condition
  (flet ((verify-cell-error-name (condition)
           (is (eq 'that-bazinga (cell-error-name condition)))))
    (signals unbound-bazinga
      (handler-bind ((unbound-bazinga #'verify-cell-error-name))
        (symbol-bazinga 'that-bazinga))))
  ;; Boundp
  (is (bazinga-boundp 'this-bazinga))
  (is (not (bazinga-boundp 'that-bazinga)))
  ;; Default values
  (is (eq :bazinga-too (symbol-bazinga 'that-bazinga :bazinga-too)))
  (is (eq :bazinga-too (symbol-bazinga 'that-bazinga)))
  ;; Binding table
  (let ((binding-table *bazinga-table*))
    (is (hash-table-p binding-table))
    (is (eq 'eq (hash-table-test binding-table)))
    (is (eq :bazinga (gethash 'this-bazinga binding-table))))
  ;; Type name
  (is (alexandria:type= 'keyword 'bazinga-type))
  ;; Documentation
  (is (symbol-bazinga 'this-bazinga))
  (is (string= "A bazinga."
               (setf (documentation 'this-bazinga 'bazinga) "A bazinga.")))
  (is (string= "A bazinga." (documentation 'this-bazinga 'bazinga)))
  ;; Documentation table
  (let ((documentation-table *bazinga-doc-table*))
    (is (hash-table-p documentation-table))
    (is (eq 'eq (hash-table-test documentation-table)))
    (is (string= "A bazinga." (gethash 'this-bazinga documentation-table))))
  ;; CLEAR-NAMESPACE
  (is (eq :bazinga (symbol-bazinga 'this-bazinga)))
  (is (eq :bazinga-too (symbol-bazinga 'that-bazinga)))
  (clear-namespace 'bazinga)
  (signals unbound-bazinga (symbol-bazinga 'this-bazinga))
  (signals unbound-bazinga (symbol-bazinga 'that-bazinga)))
