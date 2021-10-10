(in-package :stumpwm)

#+sbcl (defvar *sbcl-home* (uiop:lisp-implementation-directory))
#+sbcl (sb-posix:unsetenv "SBCL_HOME")

(defvar *stumpwm-start-hook* nil
  "Functions to run at stumpwm startup")

(defun register-stumpwm-start-hook (hook)
  "Register a hook function to be run when stumpwm starts"
  (uiop:register-hook-function '*stumpwm-start-hook* hook))

(defun call-stumpwm-start-hook ()
  "Run the hook function for when stumpwm starts"
  (uiop:call-functions *stumpwm-start-hook*))


(clear-window-placement-rules)

(setf *mouse-focus-policy* :sloppy
      *window-border-style* :thin
      *maxsize-border-width* 1
      *transient-border-width* 1
      *normal-border-width* 1
      *input-window-gravity* :top-right
      *message-window-gravity* :top-right)

(defun in-home (p)
  (uiop:unix-namestring (uiop:subpathname (user-homedir-pathname) p)))

(defun fare-defaults ()
  (set-focus-color "#00FF00")
  (set-unfocus-color "#000000")
  (setf xft:*font-dirs*
        (list (in-home ".fonts/")
              (in-home ".guix-profile/share/fonts/")
              "/run/current-system/profile/share/fonts/"
              "/usr/share/fonts/"))
  (ignore-errors (set-font "-xos4-terminus-medium-r-normal--12-240-72-72-c-60-iso8859-1"))
  (ignore-errors (set-font "-*-proggyclean-*-*-*-*-*-240-*-*-*-*-*-*"))
  (ignore-errors (set-font "-*-lucidatypewriter-*-*-*-*-*-240-*-*-*-*-*-*"))
  (ignore-errors (set-font (make-instance 'xft:font :family "CMU Typewriter Text" :subfamily "Regular" :size 26)))
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
(run-with-timer
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
