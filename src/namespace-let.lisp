
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
    (`(((,(and namespace (binding)) ,name) ,@definition) ,@rest)
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
  (ematch namespace
    ((namespace- accessor)
     (with-gensyms (temp)
       `((let ((,temp ,@definition))
           (macrolet ((,accessor (&whole whole x)
                        (if (equal x '(quote ,name))
                            ',temp
                            whole)))
             ,@body)))))))




#|

(namespace-let ((#'x (y) (1+ y))
                ((macro x) (y) (1+ y))
                ((macro y) (y) (1+ y))
                (#'x (y) (1+ y))
                ((macro y) (y) (1+ y))
                ((symbol-macro sm) 0)
                (b 0))
  (let ((b 1))
    (print :x)))

;; (PROGN
;;  (FLET ((X (Y)
;;           (1+ Y)))
;;    (MACROLET ((X (Y)
;;                 (1+ Y))
;;               (Y (Y)
;;                 (1+ Y)))
;;      (FLET ((X (Y)
;;               (1+ Y)))
;;        (MACROLET ((Y (Y)
;;                     (1+ Y)))
;;          (SYMBOL-MACROLET ((SM 0))
;;            (LET ((B 0))
;;              (PROGN
;;               (LET ((B 1))
;;                 (PRINT :X))))))))))

(namespace-let (((restart continue) (lambda (c) (declare (ignore c)) (print :hi!))))
  (let ((b 1))
    (print :x)))

;; (PROGN
;;  (RESTART-BIND ((CONTINUE (LAMBDA (C) (DECLARE (IGNORE C)) (PRINT :HI!))))
;;    (PROGN
;;     (LET ((B 1))
;;       (PRINT :X)))))

(define-namespace test fixnum)
(setf (symbol-test 'a) 0)
(print (symbol-test 'a))

;; should be 1

(funcall
 (namespace-let (((test a) 1))
   (lambda ()
     (symbol-test 'a))))

;; (FUNCALL
;;  (PROGN
;;    (LET ((#:TEMP1976 1))
;;      (MACROLET ((SYMBOL-TEST (&WHOLE WHOLE X)
;;                   (IF (EQUAL X ''A)
;;                       '#:TEMP1976
;;                       WHOLE)))
;;        (PROGN (LAMBDA () (SYMBOL-TEST 'A)))))))

|#
