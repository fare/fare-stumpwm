(in-package :stumpwm)

;;; Start up
(defun my-start ()
  (activate-terminator)
  (run-commands "grename 1" "gnewbg 2"))

(register-stumpwm-start-hook 'my-start)
