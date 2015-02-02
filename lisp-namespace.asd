#|
  This file is a part of lisp-namespace project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage lisp-namespace-asd
  (:use :cl :asdf))
(in-package :lisp-namespace-asd)


(defsystem lisp-namespace
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:alexandria :introspect-environment)
  :components ((:module "src"
                :components
                ((:file "namespace"))))
  :description ""
  :in-order-to ((test-op (load-op lisp-namespace.test))))
