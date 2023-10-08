(in-package :stumpwm)

(defun logname ()
  #+sbcl (sb-unix:uid-username (sb-unix:unix-getuid))
  #-sbcl (last (pathname-directory (user-homedir-pathname)))) ;; bad approximation?

(defun is-guix-p ()
  (and (uiop:probe-file* "/run/current-system/")
       (not (uiop:probe-file* "/etc/NIXOS"))))
(defun is-nixos-p ()
  (and (uiop:probe-file* "/run/current-system/")
       (uiop:probe-file* "/etc/NIXOS")
       t))

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
    (activate-initial-clients)
    ;;(run-commands "grename 1" "gnewbg 2")
    nil))

(defvar *initial-clients-activated* nil)

(defun activate-initial-clients ()
  (unless (or *initial-clients-activated* (all-windows))
    (setf *initial-clients-activated* t)
    (unless (is-guix-p) ;; Looks like GUIX obsoleted xscreensaver with its own system
      (run-shell-command "xscreensaver -nosplash"))
    (activate-emacs)
    (activate-terminator)
    (activate-chromium)))

(register-stumpwm-start-hook 'my-start)
