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
  :depends-on (:lisp-namespace
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) ))
