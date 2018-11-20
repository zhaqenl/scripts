;;;; utils.lisp

(uiop:define-package #:scripts/utils
    (:use #:cl
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:fare-utils
          #:cl-ppcre
          #:cl-launch/dispatch)
  (:export #:char-display-char
           #:battery-status
           #:wine
           #:err
           #:apply-args
           #:apply-args-1
           #:string-first
           #:find-binary
           #:with-qt
           #:run-with-nix-user
           #:$
           #:%))

(in-package #:scripts/utils)

(defun char-display-char (c)
  (if (or (member c '(127 155))
          (< c 32)
          (<= 128 c 159))
      #\space
      (code-char c)))

(defun wine (path &rest args)
  (run/i `(wine ,path ,@args)))

(defun err (message)
  (die 1 (format t "Error: ~A~%" message)))

(defun apply-args (function options args)
  (apply function (append (list options) args)))

(defun apply-args-1 (function args &key (options nil))
  (apply function (append options args)))

(defun string-first (string)
  (let* ((space (position #\  string :test #'equal)))
    (subseq string 0 space)))

(defun find-binary (binary)
  (run/ss `(readlink -f ,(run/ss `(which ,binary)))))

(defmacro % (name command)
  `(defun ,name (&rest args)
     (run/i (append (split "\\s+" ,command) args))
     (success)))

(defun run-with-nix-user (profile binary args)
  "Run binary under a separate profile."
  (let ((bin (mof:home (mof:fmt ".baf/profiles/~A/bin" profile))))
    (setf (getenv "PATH") (unix-namestring bin))
    (run/i `(,binary ,@args))
    (success)))

(defun with-qt (command args)
  "Run a program in the QT profile."
  (setf (getenv "QT_QPA_PLATFORMTHEME") "qt5ct")
  (run-with-nix-user "qt" command args))

(defmacro $ (command name &optional alias)
  "Define a runner in the QT profile."
  `(progn
     (defun ,name (&rest args)
       (with-qt ,command args))
     ,(when alias
        `(defun ,alias (&rest args)
           (with-qt ,command args)))))
