(in-package :stumpwm)

(defun logname ()
  #+sbcl (sb-unix:uid-username (sb-unix:unix-getuid))
  #-sbcl (last (pathname-directory (user-homedir-pathname)))) ;; bad approximation?

;;; Start up
(defun my-start ()
  (let ((hostname (uiop:hostname)))
    (cond
      ((equal hostname "flatland")
       '(inferior-shell:run '("xrandr" "--output" "eDP1" "--mode" "3200x1800" "--auto"))))
    (uiop:if-let (modmap (uiop:probe-file* (uiop:subpathname (user-homedir-pathname) ".Xmodmap")))
                 (run-shell-command (format nil "xmodmap ~A" modmap)))
    (run-shell-command
     (format nil "xrdb -cpp m4 -DLOCALSYSTEM=~a -DLOGNAME=~a -load ~a"
             hostname
             (logname)
             (uiop:subpathname (user-homedir-pathname) "etc/X11/Xdefaults.m4")))
    ;; Looks like GUIX obsoleted xscreensaver with its own system
    (run-shell-command "[ -f /etc/NIXOS ] && xscreensaver -nosplash")
    (activate-initial-clients)
    ;;(run-commands "grename 1" "gnewbg 2")
    nil))

(defvar *initial-clients-activated* nil)

(defun activate-initial-clients ()
  (unless *initial-clients-activated*
    (setf *initial-clients-activated* t)
    (activate-emacs)
    (activate-terminator)
    (activate-chromium)))

(register-stumpwm-start-hook 'my-start)
