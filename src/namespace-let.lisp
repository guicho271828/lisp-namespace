
(in-package :lispn)

;; TODO namespace-let

;; renaming candidate:
;; namespace-let 
;; where --- eazy to type in, but incosistent with common lisp
;; bind --- I dont think it is cool, similar reason to where
;; overwriting cl:let --- well, maybe optional

(named-readtables:in-readtable :fare-quasiquote)

(defmacro namespace-let (bindings &body body)
  (%pickone (reverse bindings) `((progn ,@body))))

(setf (macro-function 'nslet)
      (macro-function 'namespace-let))

;; mutual recursion

(defun %pickone (bindings body)
  (ematch bindings
    ;; function-like binding
    (`(((function ,name) ,@definition) ,@rest)
      (%merge 'flet name definition body rest))
    (`(((label ,name) ,@definition) ,@rest)
      (%merge 'labels name definition body rest))
    (`(((macro ,name) ,@definition) ,@rest)
      (%merge 'macrolet name definition body rest))
    ;; variable-like binding
    (`(((symbol-macro ,name) ,@definition) ,@rest)
      (%merge 'symbol-macrolet name definition body rest))
    (`((,(and name (type symbol)) ,@definition) ,@rest)
      (%merge 'let name definition body rest))
    ;; handler binding
    (`(((handler ,name) ,@definition) ,@rest)
      (%merge 'handler-bind name definition body rest))
    (`(((restart ,name) ,@definition) ,@rest)
      (%merge 'restart-bind name definition body rest))
    ;; namespace binding
    (`(((,(and namespace (namespace-bound)) ,name) ,@definition) ,@rest)
      (%pickone rest (%wrap namespace name definition body)))
    (`() `(progn ,@body))))

(defun %merge (kind name def body rest)
  (%pickone
   rest
   (match body
     (`((,(eq kind) (,@bindings) ,@body))
      `((,kind ((,name ,@def) ,@bindings) ,@body)))
     (_
      `((,kind ((,name ,@def)) ,@body))))))

(defun %wrap (namespace name definition body)
  (ematch (symbol-namespace namespace)
    ((%namespace- accessor type)
     (with-gensyms (temp)
       `((let ((,temp ,@definition))
           (declare (type (,type) ,temp))
           (macrolet ((,accessor (&whole whole x)
                        (if (equal x '(quote ,name))
                            ',temp
                            whole)))
             ,@body)))))))

;; lexical nickname for packages : abondoned

#+nil
(defun %bind-package (def body rest-bindings)
  ;;  This one is special. All symbols are interned in the current package at
  ;; the read time, but this binder parse them again, then intern in the
  ;; target package.
  ;;  Also, it runs in compile-time, not in runtime. Therefore, the target
  ;; package should also exist in compile-time.
  (assert (find-package def) nil
          "The specified package ~a should exist in compilation time!" def)
  (let ((pkg (find-package def)))
    (%pickone
     rest-bindings
     (maptree (lambda (s)
                (match s
                  ((symbol name)
                   (intern name pkg))
                  (_ s)))
              body))))

#+nil
(defun maptree (fn tree)
  (match tree
    ((cons car cdr)
     (cons (maptree fn car)
           (maptree fn cdr)))
    ((type array)
     (let ((a (copy-array tree)))
       (dotimes (i (array-total-size a) a)
         (setf (row-major-aref a i)
               (funcall fn (row-major-aref tree i))))))
    (_ (funcall fn tree))))

#+nil
(maptree #'print '(let (x y)
                   (test-let ((a 1))
                     (setf x (lambda () (symbol-test 'a)))
                     (test-let ((a 2))
                       (setf y (lambda () (symbol-test 'a)))))
                   (is (= 1 (funcall x)))
                   (is (= 2 (funcall y)))))
#+nil
(maptree #'print #(a b c d e))
#+nil
(maptree #'print #2a((a b c d e)))
#+nil
(read-from-string "`(a ,b)")

