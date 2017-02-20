":" ; exec cl-launch -Q -sm cl-scripts/touchpad "$0" "$@"
;; -*- lisp -*-
;; Based on https://wiki.archlinux.org/index.php/Touchpad_Synaptics#Software_toggle
;; Use the UI preferences to add a keyboard shortcut that invokes this script.
;; To avoid the slow startup time of lisp as a script, better dump an image with:
;;   cl-launch -o ~/bin/x64/touchpad -d ! -l clisp \
;;     -s optima.ppcre -s inferior-shell -E touchpad::main -L touchpad.lisp
;; Or use make-multi.sh to create a multi-call binary that includes touchpad support.

(uiop:define-package
    :cl-scripts/touchpad
    (:use :cl
          :fare-utils
          :uiop
          :inferior-shell
          :optima
          :optima.ppcre
          :cl-scripting)
  (:export #:help
           #:get-touchpad-id
           #:device-enabled-p
           #:toggle-device
           #:disable-device
           #:enable-device))

(in-package :cl-scripts/touchpad)

(defun get-touchpad-id ()
  (dolist (line (run/lines '(xinput list)))
    (match line
           ((ppcre "(TouchPad|\\sSYNA.*)\\s+id\=([0-9]{1,2})\\s+" _ x)
            (return (values (parse-integer x)))))))

(defun device-enabled-p (&optional (id (get-touchpad-id)))
  (dolist (line (run/lines `(xinput list-props ,id)))
    (match line
           ((ppcre "Device Enabled\\s+[():0-9]+\\s+([01])" x) (return (equal x "1"))))))

(defun toggle-device (&optional (id (get-touchpad-id)) (on :toggle))
  (let ((state (ecase on
                 ((:toggle) (not (device-enabled-p id)))
                 ((nil t) on))))
    (run `(xinput ,(if state 'enable 'disable) ,id)))
  (success))

(defun enable-device (&optional (id (get-touchpad-id)))
  (toggle-device id t))

(defun disable-device (&optional (id (get-touchpad-id)))
  (toggle-device id nil))

(defun help (&optional (output *standard-output*))
  (format output "touchpad functions: ~{~(~A~)~^ ~}~%"
          (package-functions :cl-scripts/touchpad))
  (success))

(defun main (argv) ;; TODO: use command-line-arguments, or CLON
  (cond
    ((null argv) (toggle-device))
    ((eql (first-char (first argv)) #\() (eval (first argv)))
    (t (if-let (fun (package-function :cl-scripts/touchpad
                                      (standard-case-symbol-name (first argv))))
           (apply 'run-command fun (rest argv))
         (progn
           (format *error-output* "Bad touchpad command: ~A~%" (first argv))
           (help *error-output*)
           (quit 2))))))