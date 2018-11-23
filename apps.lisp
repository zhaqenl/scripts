;;;; apps.lisp

(uiop:define-package #:scripts/apps
    (:use #:cl
          #:fare-utils
          #:uiop
          #:inferior-shell
          #:cl-scripting
          #:optima
          #:optima.ppcre
          #:cl-launch/dispatch
          #:scripts/misc
          #:scripts/utils
          #:scripts/unix)
  (:export #:s
           #:e
           #:oa-x
           #:oa
           #:a0

           #:calibre
           #:qbittorrent
           
           #:vbox))

(in-package #:scripts/apps)

(defvar +screenshots-dir+ (mof:home ".screenshots"))

(exporting-definitions
 (% s "sudo")
 (% e "emacsclient -nw")
 (% oa-x "xhost local:root")
 (% oa "docker run --rm -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix ebzzry/opera /usr/bin/opera --no-sandbox")
 (% a0 "xmodmap /home/zhaqenl/.Xmodmap"))

(exporting-definitions
 ($ "calibre" calibre)
 ($ "qbittorrent" qbittorrent))

(exporting-definitions
 (defun vbox () (run-with-nix-system "VirtualBox")))

(register-commands :scripts/apps)
