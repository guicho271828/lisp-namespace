#|
  This file is a part of lisp-namespace project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage lisp-namespace.test-asd
  (:use :cl :asdf))
(in-package :lisp-namespace.test-asd)


(defsystem lisp-namespace.test
  :author "Masataro Asai"
  :license "LLGPL"
  :defsystem-depends-on (:fiveam)
  :depends-on (:lisp-namespace :iterate)
  :components ((:module "t"
                :components
                ((:file "package"))
                :serial t))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(5am:run! :lisp-namespace)"))
		    (asdf:clear-system c)))
