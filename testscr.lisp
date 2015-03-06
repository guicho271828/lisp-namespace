
(in-package :cl-user)

(defun test (sys)
  (handler-case
      (progn
        (ql:quickload sys)
        (asdf:test-system sys)
        (fiveam:run sys))
    (serious-condition (c)
      (describe c)
      (uiop:quit 2))))

(uiop:quit (if (every #'fiveam::TEST-PASSED-P
                      (test :lisp-namespace))
               0 1))
