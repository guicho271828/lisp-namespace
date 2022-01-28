;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(asdf:defsystem #:lisp-namespace
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
               (:file "define-namespace-short")
               (:file "define-namespace-long")
               (:file "define-namespace")
               (:file "describe-object")
               ;; TODO: remove this altogether
               ;; (:file "namespace-let")
               (:file "documentation"))
  :description "Utilities for extensible namespaces in Common Lisp."
  :in-order-to ((test-op (load-op #:lisp-namespace/test)))
  :perform (test-op (op c)
             (let ((suite-name (find-symbol (symbol-name '#:lisp-namespace)
                                            '#:lisp-namespace/test)))
               (uiop:symbol-call '#:5am '#:run! suite-name))))

(defsystem #:lisp-namespace/test
  :author ("Masataro Asai <guicho2.71828@gmail.com>"
           "Michał \"phoe\" Herda <phoe@disroot.org>")
  :mailto "guicho2.71828@gmail.com"
  :description "test system for lisp-namespace"
  :license "LLGPL"
  :depends-on (#:lisp-namespace #:uiop #:fiveam)
  :pathname "t"
  :components ((:file "lisp-namespace"))
  :serial t)
