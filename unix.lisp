;;;; unix.lisp

(uiop:define-package #:scripts/unix
    (:use #:cl
          #:fare-utils
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:optima
          #:optima.ppcre
          #:cl-launch/dispatch
          #:scripts/misc
          #:scripts/utils)
  (:export #:l
           #:ll))

(in-package #:scripts/unix)

(exporting-definitions
 (% l  "la -tr")
 (% ll "l -l"))

(register-commands :scripts/unix)
