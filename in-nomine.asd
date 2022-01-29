;;;; This file is a part of IN-NOMINE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(asdf:defsystem #:in-nomine
  :version "1.0"
  :author ("Masataro Asai <guicho2.71828@gmail.com>"
           "Michał \"phoe\" Herda <phoe@disroot.org>")
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (#:alexandria)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "namespace")
               (:file "definers")
               (:module "define-namespace"
                :components ((:file "common")
                             (:file "short")
                             (:file "long")
                             (:file "macro")))
               (:file "describe-object")
               (:file "documentation"))
  :description "Utilities for extensible namespaces in Common Lisp."
  :in-order-to ((test-op (load-op #:in-nomine/test)))
  :perform (test-op (op c)
             (uiop:symbol-call '#:5am '#:run!
                               (find-symbol (symbol-name '#:in-nomine)
                                            '#:in-nomine/test))))

(defsystem #:in-nomine/test
  :author ("Masataro Asai <guicho2.71828@gmail.com>"
           "Michał \"phoe\" Herda <phoe@disroot.org>")
  :mailto "guicho2.71828@gmail.com"
  :description "test system for in-nomine"
  :license "LLGPL"
  :depends-on (#:in-nomine
               #:alexandria
               #:uiop
               #:fiveam
               #:introspect-environment
               #:closer-mop)
  :pathname "t"
  :components ((:file "in-nomine"))
  :serial t)
