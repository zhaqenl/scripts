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
           #:par
           #:oa-x
           #:oa
           #:a0
           #:qb
           #:qt5ct
           #:td

           #:vbox
           #:cb

           #:shell
           #:rshell

           #:screenshot))

(in-package #:scripts/apps)

(defvar +screenshots-dir+ (mof:home ".screenshots"))

(defun run-with-locale (locale &rest args)
  "Run args with locale set to LOCALE"
  (setf (getenv "LANG") locale)
  (run/i `(,@(first args)))
  (success))

(defun run-with-nix-user (profile binary &rest args)
  "Run binary under a separate profile"
  (let ((bin (mof:home (mof:fmt ".baf/profiles/~A/bin" profile))))
    (setf (getenv "PATH") (unix-namestring bin))
    (run/i `(,binary ,@args))))

(defun run-with-nix-system (binary &rest args)
  "Run binary without user paths"
  (setf (getenv "PATH") "/var/setuid-wrappers:/run/wrappers/bin:/run/current-system/sw/bin:/run/current-system/sw/sbin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin")
  (run/i `(,binary ,@args)))

(defun run-with-xdg (binary &rest args)
  "Run binary under a custom XDG_DATA_DIRS path"
  (setf (getenv "XDG_DATA_DIRS")
        (uiop:native-namestring (mof:home ".local/share/mime")))
  (run/i `(,binary ,@args)))

(exporting-definitions
 (% s "sudo")
 (% e "emacsclient -nw")
 (% par "parallel --will-cite")
 (% oa-x "xhost local:root")
 (% oa "docker run --rm -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix ebzzry/opera /usr/bin/opera --no-sandbox")
 (% a0 "xmodmap /home/zhaqenl/.Xmodmap"))

(exporting-definitions
 ($ "qt5ct" qt5ct)
 ($ "qbittorrent" qbittorrent qb)
 ($ "telegram-desktop" telegram-desktop td))

(exporting-definitions
 (defun vbox () (run-with-nix-system "VirtualBox"))
 (defun cb (&rest args) (run-with-nix-user "calibre" "calibre" args)))

(exporting-definitions
 (defun shell (&rest args)
   (let ((directory (pathname-directory-pathname (find-binary (argv0)))))
     (run/i `(nix-shell --pure ,(mof:fmt "~A/default.nix" directory) ,@args))
     (success)))

 (defun rshell (command)
   (shell "--command" (mof:fmt " rlwrap -s 1000000 -c -b \"(){}[].,=&^%0\;|\" ~A" command)))

 (defun screenshot (mode)
   (let* ((dir (uiop:truenamize +screenshots-dir+))
          (file (mof:fmt "~A.png" (local-time:format-timestring nil (local-time:now))))
          (dest (mof:fmt "mv $f ~A" dir))
          (image (mof:fmt "~A~A" dir file)))
     (flet ((scrot (file dest &rest args)
              (run/i `(scrot ,@args ,file -e ,dest))))
       (match mode
              ((ppcre "(full)") (scrot file dest))
              ((ppcre "(region)") (scrot file dest '-s))
              (_ (err (mof:fmt "invalid mode ~A~%" mode))))
       (run `("xclip" "-selection" "clipboard") :input (list image))
       (success)))))

(register-commands :scripts/apps)
