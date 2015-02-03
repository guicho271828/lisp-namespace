#|
  This file is a part of lisp-namespace project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :lisp-namespace.test
  (:use :cl
        :lisp-namespace
        :fiveam
        :iterate :alexandria))
(in-package :lisp-namespace.test)



(def-suite :lisp-namespace)
(in-suite :lisp-namespace)

;; run test with (run! test-name) 
;;   test as you like ...

(test let
  (signals error
    (eval
     '(let ((b 0)
            (b 1))
       (print :x))))
  ;; does not merge (b 0) and (b 1)
  (finishes
    (namespace-let ((b 0))
      (let ((b 1))
        (print :x)))))

(test complicated
  (finishes
    `(namespace-let ((#'x (y) (1+ y))
                     ((macro x) (y) (1+ y))
                     ((macro y) (y) (1+ y))
                     ((label z) (y) (w y))
                     ((label w) (y) (z y))
                     ((macro y) (y) (1+ y))
                     ((symbol-macro sm) 0)
                     (b 0))
       (let ((b 1))
         (print :x)))))

(test complicated
  (finishes
    (namespace-let (((restart continue)
                     (lambda (c)
                       (declare (ignore c))
                       (print :hi!))))
      (let ((b 1))
        (print :x)))))

(define-namespace test fixnum)

(test namespace
  (finishes
    (setf (symbol-test 'a) 0))
  (is (= (symbol-test 'a)))
  (signals unbound-test
    (symbol-test 'b))
  ;; lexical
  (is (= 1
         (funcall
          (namespace-let (((test a) 1))
            (lambda ()
              (symbol-test 'a))))))
  (let (x)
    (namespace-let (((test a) 1))
      (setf x 
            (lambda ()
              (symbol-test 'a))))
    (is (= 1 (funcall x)))))

(5am:run! :lisp-namespace)
