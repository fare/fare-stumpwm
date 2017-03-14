(in-package :stumpwm)

(defvar *swank-port*
  4004
  "The default port for swank")

(defun ensure-swank ()
  (let ((port *swank-port*))
    (unless (inferior-shell:run/lines `(lsof -i (":" ,port)) :on-error nil)
      (swank-loader:init)
      (swank:create-server :port port
                           :style swank:*communication-style*
                           :dont-close t))))

(defcommand start-swank ()
  ()
  "Start Swank"
  (ensure-swank))

(register-stumpwm-start-hook 'start-swank)
