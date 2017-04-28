(in-package :lispn)

(define-namespace namespace %namespace nil "A namespace for managing namespaces themselves.")

(defun clear-namespace (name)
  (assert (gethash name *namespace-table*))
  (clrhash (symbol-value (%namespace-hash (gethash name *namespace-table*))))
  name)

#+clisp
(format *error-output* "On CLISP, we cannot add method to DESCRIBE-OBJECT, so you cannot enjoy extended documentations for various namespaces")

#-clisp ;; Lispworks complains about redefining existing describe-object method. Let's continue.
(handler-bind
    ((simple-error #'(lambda (c)
		       (invoke-restart (find-restart 'continue c)))))
  (defmethod describe-object :after ((x symbol) s)
             (let ((*print-pretty* t))
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
                          *namespace-table*)))))
