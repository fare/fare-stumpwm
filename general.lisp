(in-package :stumpwm)

#+sbcl (defvar *sbcl-home* (uiop:lisp-implementation-directory))
#+sbcl (sb-posix:unsetenv "SBCL_HOME")

(defun register-stumpwm-start-hook (hook)
  "Register a hook function to be run when stumpwm starts"
  (add-hook *start-hook* hook))

(setf *mouse-focus-policy* :sloppy
      *window-border-style* :thin
      *maxsize-border-width* 1
      *transient-border-width* 1
      *normal-border-width* 1
      *input-window-gravity* :top-right
      *message-window-gravity* :top-right)

(defun in-home (p)
  (uiop:unix-namestring (uiop:subpathname (user-homedir-pathname) p)))

(defvar *orig-font-dirs* xft:*font-dirs*)

(defun fare-defaults ()
  (clear-window-placement-rules)
  (set-focus-color "#00FF00")
  (set-unfocus-color "#000000")
  (setf xft:*font-dirs*
        (list (in-home ".fonts/") ;; HOME
              (in-home ".nix-profile/share/X11/fonts/") ;; NixOS
              "/run/current-system/sw/share/X11/fonts/" ;; NixOS
              (in-home ".guix-profile/share/fonts/") ;; GUIX
              "/run/current-system/profile/share/fonts/" ;; GUIX
              "/usr/share/fonts/")) ;; Debian
  ;;(clx-truetype:cache-fonts)
  (dolist (x (list ;; "-xos4-terminus-medium-r-normal--12-240-72-72-c-60-iso8859-1"
                   (ignore-errors (make-instance 'xft:font :family "CMU Typewriter Text" :subfamily "Bold" :size 12))
                   "-misc-ubuntu mono-bold-r-normal--32-0-0-0-m-0-iso10646-1"
                   "-*-lucidatypewriter-*-*-*-*-*-240-*-*-*-*-*-*"))
    (when (ignore-errors (set-font x)) (return)))
  (setf *startup-message* "Welcome to Faré's StumpWM")
  nil)

(register-stumpwm-start-hook 'fare-defaults)

(defun do-shell-command (command)
  "Run a shell command and display output to screen.
This must be used in a functional side-effects-free style!
If a program does not exit of its own accord, Stumpwm might hang!"
  (check-type command string)
  (echo-string (current-screen) (run-shell-command command t)))

(define-stumpwm-command "shell-command" ((command :string "sh: " :string))
  (check-type command string)
  (do-shell-command command))

;; Work around memory leak in cl-truetype
;; https://github.com/stumpwm/stumpwm/issues/474#issuecomment-481885037
'(run-with-timer
 900 900
 (lambda ()
   (loop for font in (stumpwm::screen-fonts (current-screen))
         when (typep font 'xft:font)
           do (clrhash (xft::font-string-line-bboxes font))
              (clrhash (xft::font-string-line-alpha-maps font))
              (clrhash (xft::font-string-bboxes font))
              (clrhash (xft::font-string-alpha-maps font)))))

;; see also https://github.com/ivoarch/.dot-org-files/blob/master/stumpwm.org
;; and https://github.com/alezost/stumpwm-config/find/master
