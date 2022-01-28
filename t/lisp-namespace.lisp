;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)


(defpackage #:lisp-namespace/test
  (:use #:cl
        #:lisp-namespace
        #:fiveam)
  ;; Private symbols for testing.
  (:import-from #:lisp-namespace
                #:*namespaces*
                #:ensure-namespace)
  (:export #:lisp-namespace))

(in-package #:lisp-namespace/test)

(def-suite lisp-namespace)
(in-suite lisp-namespace)

(test internal-structure
  (let* ((ns1 *namespaces*)
         (ns2 (symbol-namespace 'namespace))
         (ns3 (ensure-namespace 'namespace))
         (ns4 (gethash 'namespace (namespace-hash-table ns1)))
         (ns5 (gethash 'namespace (namespace-hash-table ns2)))
         (ns6 (gethash 'namespace (namespace-hash-table ns3))))
    (is (eq ns1 ns2))
    (is (eq ns1 ns3))
    (is (eq ns1 ns4))
    (is (eq ns1 ns5))
    (is (eq ns1 ns6))))

(test meta-namespace
  (let ((namespace *namespaces*))
    ;; Name
    (is (eq 'namespace (namespace-name namespace)))
    ;; Accessor
    (let ((accessor (namespace-accessor namespace)))
      (is (eq 'symbol-namespace accessor))
      (is (eq namespace (funcall accessor 'namespace)))
      (funcall (fdefinition `(setf ,accessor)) namespace 'some-other-namespace)
      (is (eq namespace (funcall accessor 'some-other-namespace)))
      ;; Condition
      (let ((condition-name (namespace-condition-name namespace)))
        (is (eq 'unbound-namespace condition-name))
        (flet ((verify-cell-error-name (condition)
                 (is (eq 'yet-another-namespace (cell-error-name condition)))))
          (signals unbound-namespace
            (handler-bind ((unbound-namespace #'verify-cell-error-name))
              (funcall (funcall accessor 'yet-another-namespace)))))
        ;; Boundp
        (let ((boundp-symbol (namespace-boundp-symbol namespace)))
          (is (eq 'namespace-boundp boundp-symbol))
          (is (funcall boundp-symbol 'namespace))
          (is (funcall boundp-symbol 'some-other-namespace))
          (is (null (funcall boundp-symbol 'yet-another-namespace)))
          ;; Makunbound
          (let ((makunbound-symbol (namespace-makunbound-symbol namespace)))
            (is (eq 'namespace-makunbound makunbound-symbol))
            (funcall makunbound-symbol 'some-other-namespace)
            (is (null (funcall boundp-symbol 'some-other-namespace)))))))
    ;; Type name
    (let ((type-name (namespace-type-name namespace)))
      (is (eq 'namespace-type type-name))
      (is (alexandria:type= 'namespace type-name)))
    ;; Type
    (let ((value-type (namespace-value-type namespace)))
      (is (eq 'namespace value-type)))
    ;; Hash table
    (let ((hash-table (namespace-hash-table namespace)))
      (is (hash-table-p hash-table))
      (is (eq namespace (gethash 'namespace hash-table))))
    ;; Documentation table
    (let ((documentation-table (namespace-documentation-table namespace)))
      (is (hash-table-p documentation-table))
      (is (string= "A namespace for managing namespaces."
                   (gethash 'namespace documentation-table))))))

(test define-namespace-short-form
  ;; NOTE: HANDLER-BIND is to muffle redefinition warnings which may happen
  ;;       when reevaluating DEFINE-NAMESPACE when running the test suite
  ;;       multiple times in a single Lisp image.
  (let ((namespace (handler-bind ((warning #'muffle-warning))
                     (define-namespace thing keyword))))
    ;; Return value of DEFINE-NAMESPACE
    (is (typep namespace 'namespace))
    ;; Name
    (is (eq 'thing (namespace-name namespace)))
    ;; Accessor
    (let ((accessor (namespace-accessor namespace)))
      (is (eq 'symbol-thing accessor))
      (funcall (fdefinition `(setf ,accessor)) :thing 'this-thing)
      (is (eq :thing (funcall accessor 'this-thing)))
      ;; Condition
      (let ((condition-name (namespace-condition-name namespace)))
        (is (eq 'unbound-thing condition-name))
        (flet ((verify-cell-error-name (condition)
                 (is (eq 'that-thing (cell-error-name condition)))))
          (signals unbound-thing
            (handler-bind ((unbound-thing #'verify-cell-error-name))
              (funcall (funcall accessor 'that-thing)))))
        ;; Hash table
        (let ((hash-table (namespace-hash-table namespace)))
          (is (hash-table-p hash-table))
          (is (eq :thing (gethash 'this-thing hash-table))))
        ;; Boundp
        (let ((boundp-symbol (namespace-boundp-symbol namespace)))
          (is (eq 'thing-boundp boundp-symbol))
          (is (funcall boundp-symbol 'this-thing))
          (is (null (funcall boundp-symbol 'that-thing)))
          ;; Makunbound
          (let ((makunbound-symbol (namespace-makunbound-symbol namespace)))
            (is (eq 'thing-makunbound makunbound-symbol))
            (funcall makunbound-symbol 'this-thing)
            (is (null (funcall boundp-symbol 'this-thing))))))
      ;; CLEAR-NAMESPACE
      (funcall (fdefinition `(setf ,accessor)) :thing 'this-thing)
      (funcall (fdefinition `(setf ,accessor)) :nothing 'that-thing)
      (is (eq :thing (funcall accessor 'this-thing)))
      (is (eq :nothing (funcall accessor 'that-thing)))
      (clear-namespace 'thing)
      (signals unbound-thing (funcall accessor 'this-thing))
      (signals unbound-thing (funcall accessor 'that-thing)))
    ;; Type name
    (let ((type-name (namespace-type-name namespace)))
      (is (eq 'thing-type type-name))
      (is (alexandria:type= 'keyword type-name)))
    ;; Type
    (let ((value-type (namespace-value-type namespace)))
      (is (eq 'keyword value-type)))
    ;; Documentation
    (is (eq "A thing." (setf (documentation 'this-thing 'thing) "A thing.")))
    (is (eq "A thing." (documentation 'this-thing 'thing)))
    ;; Documentation table
    (let ((documentation-table (namespace-documentation-table namespace)))
      (is (hash-table-p documentation-table))
      (is (string= "A thing." (gethash 'this-thing documentation-table))))))

;; TODO tests for reader restarts

;; TODO long form test

;; TODO long form missing functions test

;; TODO tests for definition errors
