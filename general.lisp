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

(defun fare-defaults ()
  (set-focus-color "#00FF00")
  (set-unfocus-color "#000000")
  (set-font "-xos4-terminus-medium-r-normal--12-120-72-72-c-60-iso8859-9")
  ;;(set-font "-*-proggyclean-*-*-*-*-*-*-*-*-*-*-*-*")
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
