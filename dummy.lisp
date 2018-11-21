(uiop:define-package #:scripts/dummy
    (:use #:cl
          #:uiop
          #:inferior-shell
          #:cl-scripting))

(in-package #:scripts/dummy)

(defun main ()
  nil)
