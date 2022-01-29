;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)


(defpackage #:in-nomine/test
  (:use #:cl
        #:in-nomine
        #:fiveam)
  ;; Private symbols for testing.
  (:import-from #:in-nomine
                #:*namespaces*
                #:ensure-namespace)
  (:export #:in-nomine))

(in-package #:in-nomine/test)

(def-suite in-nomine)
(in-suite in-nomine)

(test internal-structure
  (let* ((ns1 *namespaces*)
         (ns2 (symbol-namespace 'namespace))
         (ns3 (ensure-namespace 'namespace))
         (ns4 (gethash 'namespace (namespace-binding-table ns1)))
         (ns5 (gethash 'namespace (namespace-binding-table ns2)))
         (ns6 (gethash 'namespace (namespace-binding-table ns3))))
    (is (eq ns1 ns2))
    (is (eq ns1 ns3))
    (is (eq ns1 ns4))
    (is (eq ns1 ns5))
    (is (eq ns1 ns6))))

(test metanamespace-accessors
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
    ;; Behavior booleans
    (let ((errorp (namespace-error-when-not-found-p namespace))
          (errorpp (namespace-errorp-arg-in-accessor-p namespace))
          (defaultp (namespace-default-arg-in-accessor-p namespace)))
      (is (eq 't errorp)
          (eq 'nil errorpp)
          (eq 't defaultp)))
    ;; Hash table
    (let ((hash-table (namespace-binding-table namespace)))
      (is (hash-table-p hash-table))
      (is (eq namespace (gethash 'namespace hash-table))))
    ;; Documentation type
    (let ((documentation-type (namespace-documentation-type namespace)))
      (is (eq 'namespace documentation-type)))
    ;; Documentation table
    (let ((documentation-table (namespace-documentation-table namespace)))
      (is (hash-table-p documentation-table))
      (is (string= "A namespace for managing namespaces."
                   (gethash 'namespace documentation-table))))
    ;; Documentation
    (is (string= "A namespace for managing namespaces."
                 (documentation 'namespace 'namespace)))
    ;; TODO fix this
    ;; (is (string= "A namespace for managing namespaces."
    ;;              (documentation namespace 't)))
    ))

(test short-form
  ;; NOTE: HANDLER-BIND is to muffle redefinition warnings which may happen
  ;;       when reevaluating DEFINE-NAMESPACE when running the test suite
  ;;       multiple times in a single Lisp image.
  (let ((namespace (handler-bind ((warning #'muffle-warning))
                     (define-namespace thing keyword nil
                       "A thing namespace."))))
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
        (let ((hash-table (namespace-binding-table namespace)))
          (is (hash-table-p hash-table))
          (is (eq 'eq (hash-table-test hash-table)))
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
            (is (null (funcall boundp-symbol 'this-thing))))
          ;; Accessor default arg
          (is (eq :thing (funcall accessor 'this-thing :thing)))
          (is (eq :thing (funcall accessor 'this-thing)))))
      ;; CLEAR-NAMESPACE
      (funcall (fdefinition `(setf ,accessor)) :nothing 'that-thing)
      (is (eq :thing (funcall accessor 'this-thing)))
      (is (eq :nothing (funcall accessor 'that-thing)))
      (clear-namespace 'thing)
      (signals unbound-thing (funcall accessor 'this-thing))
      (signals unbound-thing (funcall accessor 'that-thing))
      ;; Restarts
      (flet ((handle (c) (use-value :thing c)))
        (is (eq :thing (handler-bind ((unbound-thing #'handle))
                         (funcall accessor 'this-thing)))))
      (signals unbound-thing (funcall accessor 'this-thing))
      (flet ((handle (c) (store-value :thing c)))
        (is (eq :thing (handler-bind ((unbound-thing #'handle))
                         (funcall accessor 'this-thing)))))
      (is (eq :thing (funcall accessor 'this-thing))))
    ;; Hash table test
    (let ((test (namespace-hash-table-test namespace)))
      (eq test 'eq))
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
      (is (eq 'eq (hash-table-test documentation-table)))
      (is (string= "A thing." (gethash 'this-thing documentation-table))))
    ;; Documentation
    ;; TODO fix this
    ;; (is (string= "A thing namespace." (documentation namespace 'namespace)))
    ))

(test short-form-deprecation-warning
  (block nil
    (flet ((handle (condition)
             (is (typep condition 'warning))
             (is (string= (princ-to-string condition)
                          (format nil "Deprecated option BINDING true in ~
                                       DEFINE-NAMESPACE: no binding form ~
                                       was generated.")))
             (return-from nil)))
      (handler-bind ((warning #'handle))
        (macroexpand-1 '(define-namespace test-warning-namespace t t)))
      (error "Test failure: no warning was signaled"))))

;; TODO test default values of long form

(test long-form-default-values
  ;; NOTE: HANDLER-BIND is to muffle redefinition warnings which may happen
  ;;       when reevaluating DEFINE-NAMESPACE when running the test suite
  ;;       multiple times in a single Lisp image.
  (let ((namespace (handler-bind ((warning #'muffle-warning))
                     (define-namespace default-values
                       ;; A single keyword argument is required to trigger
                       ;; the long form.
                       ;; :VALUE-TYPE 'T
                       :value-type 't))))
    namespace))

(test long-form-customized
  ;; NOTE: HANDLER-BIND is to muffle redefinition warnings which may happen
  ;;       when reevaluating DEFINE-NAMESPACE when running the test suite
  ;;       multiple times in a single Lisp image.
  (let ((namespace (handler-bind ((warning #'muffle-warning))
                     (define-namespace stuff
                       :name-type string
                       :value-type string
                       :accessor string-stuff
                       :condition-name not-enough-stuff
                       :type-name stuff
                       :makunbound-symbol yeet-stuff
                       :boundp-symbol stuff-exists-p
                       :documentation-type some-stuff
                       :error-when-not-found-p t
                       :errorp-arg-in-accessor-p t
                       :default-arg-in-accessor-p t
                       :hash-table-test equal))))
    ;; Return value of DEFINE-NAMESPACE
    (is (typep namespace 'namespace))
    ;; Name
    (is (eq 'stuff (namespace-name namespace)))
    ;; Accessor
    (let ((accessor (namespace-accessor namespace)))
      (is (eq 'string-stuff accessor))
      (funcall (fdefinition `(setf ,accessor)) "value-1" "key-1")
      (is (eq "value-1" (funcall accessor "key-1")))
      ;; Condition
      (let ((condition-name (namespace-condition-name namespace)))
        (is (eq 'not-enough-stuff condition-name))
        (flet ((verify-cell-error-name (condition)
                 (is (equal "key-2" (cell-error-name condition)))))
          (signals not-enough-stuff
            (handler-bind ((not-enough-stuff #'verify-cell-error-name))
              (funcall (funcall accessor "key-2")))))
        ;; Hash table
        (let ((hash-table (namespace-binding-table namespace)))
          (is (hash-table-p hash-table))
          (is (eq "value-1" (gethash "key-1" hash-table))))
        ;; Boundp
        (let ((boundp-symbol (namespace-boundp-symbol namespace)))
          (is (eq 'stuff-exists-p boundp-symbol))
          (is (funcall boundp-symbol "key-1"))
          (is (null (funcall boundp-symbol "key-2")))
          ;; Makunbound
          (let ((makunbound-symbol (namespace-makunbound-symbol namespace)))
            (is (eq 'yeet-stuff makunbound-symbol))
            (funcall makunbound-symbol "key-1")
            (is (null (funcall boundp-symbol "key-1")))))
        ;; Accessor errorp arg here
        (is (null (funcall accessor "key-1" nil)))
        ;; Accessor default arg
        (is (eq "value-1" (funcall accessor "key-1" nil "value-1")))
        (is (eq "value-1" (funcall accessor "key-1" nil))))
      ;; CLEAR-NAMESPACE
      (funcall (fdefinition `(setf ,accessor)) "value-2" "key-2")
      (is (eq "value-1" (funcall accessor "key-1")))
      (is (eq "value-2" (funcall accessor "key-2")))
      (clear-namespace 'stuff)
      (signals not-enough-stuff (funcall accessor "key-1"))
      (signals not-enough-stuff (funcall accessor "key-2"))
      ;; Restarts
      (flet ((handle (c) (use-value "value" c)))
        (is (eq "value" (handler-bind ((not-enough-stuff #'handle))
                          (funcall accessor "key")))))
      (signals not-enough-stuff (funcall accessor "key"))
      (flet ((handle (c) (store-value "value" c)))
        (is (eq "value" (handler-bind ((not-enough-stuff #'handle))
                          (funcall accessor "key")))))
      (is (eq "value" (funcall accessor "key"))))
    ;; Type name
    (let ((type-name (namespace-type-name namespace)))
      (is (eq 'stuff type-name))
      (is (alexandria:type= 'string type-name)))
    ;; Type
    (let ((value-type (namespace-value-type namespace)))
      (is (eq 'string value-type)))
    ;; Documentation
    (is (string= "docs" (setf (documentation "key" 'some-stuff) "docs")))
    (is (string= "docs" (documentation "key" 'some-stuff)))
    ;; Documentation table
    (let ((documentation-table (namespace-documentation-table namespace)))
      (is (hash-table-p documentation-table))
      (is (string= "docs" (gethash "key" documentation-table))))))

;; TODO test long form

;; TODO test long form null arguments

;; TODO test definition errors

;; TODO test ERROR-WHEN-NOT-FOUND-P
;; TODO test ERROR-WHEN-NOT-FOUND-P with null CONDITION-NAME

;; TODO test ERRORP-ARG-IN-ACCESSOR-P
;; TODO test ERRORP-ARG-IN-ACCESSOR-P with null CONDITION-NAME

;; TODO test DEFAULT-ARG-IN-ACCESSOR-P
