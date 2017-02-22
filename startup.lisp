(in-package :stumpwm)

;;; Start up
(defun my-start ()
  (let ((hostname (inferior-shell:run/ss '("hostname"))))
    (cond
      ((equal hostname "flatland")
       (inferior-shell:run '("xrandr" "--output" "eDP1" "--mode" "3200x1800" "--auto")))))
  (activate-terminator)
  (run-shell-command "xscreensaver -nosplash")
  (run-commands "grename 1" "gnewbg 2"))

(register-stumpwm-start-hook 'my-start)
