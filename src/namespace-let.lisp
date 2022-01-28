;;;; This file is a part of LISP-NAMESPACE.
;;;; Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com),
;;;;               2022 Michał "phoe" Herda (phoe@disroot.org)

(in-package #:lisp-namespace)

;; TODO delete this file

(defmacro namespace-let (bindings &body body)
  (%pickone (reverse bindings) `((progn ,@body))))

(defmacro nslet (bindings &body body)
  (%pickone (reverse bindings) `((progn ,@body))))

;; mutual recursion

(defun %pickone (bindings body)
  (if bindings
      (destructuring-bind ((specifier &rest definition) &rest rest) bindings
        (cond
          ((consp specifier)
           (destructuring-bind (namespace name) specifier
             (case namespace
               (namespace
                (error "Attempted to bind a local namespace ~S." name))
               ;; function-like binding
               (function
                (%merge 'flet name definition body rest))
               (label
                (%merge 'labels name definition body rest))
               (macro
                (%merge 'macrolet name definition body rest))
               ;; variable-like binding
               (symbol-macro
                (%merge 'symbol-macrolet name definition body rest))
               ;; handler binding
               (handler
                (%merge 'handler-bind name definition body rest))
               (restart
                (%merge 'restart-bind name definition body rest))
               (t
                (if (namespace-boundp namespace)
                    (%pickone rest (%wrap namespace name definition body))
                    (error "unknown namespace ~a !" namespace))))))
          ((symbolp specifier)
           (%merge 'let specifier definition body rest))))
      `(progn ,@body)))

(defun %merge (kind name def body rest)
  (%pickone
   rest
   (handler-case
       (destructuring-bind ((kind2 bindings &rest newbody)) body
         (assert (eq kind kind2))
         `((,kind ((,name ,@def) ,@bindings) ,@newbody)))
     (error ()
       `((,kind ((,name ,@def)) ,@body))))))

(defun %wrap (namespace name definition body)
  (with-slots (accessor type-name) (symbol-namespace namespace)
    (with-gensyms (temp)
      `((let ((,temp ,@definition))
          (declare (type ,type-name ,temp))
          (macrolet ((,accessor (&whole whole x)
                       (if (equal x '(quote ,name))
                           ',temp
                           whole)))
            ,@body))))))

