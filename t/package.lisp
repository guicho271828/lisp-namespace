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

(test lisp-namespace

  )


