(in-package :stumpwm-user)

;; Show current time
(defcommand status () ()
  "show current time and other status"
  (message
   (uiop:with-output (s nil)
     ;; Show time
     (local-time:format-rfc1123-timestring s (local-time:now))
     (format s "~%~A~%" (machine-instance))
     ;; Show network connections
     (fare-scripts/network:wireless-connection-status s)
     ;; Show battery status
     (fare-scripts/shell-aliases:battery-status s))))

(defun set-timezone (tz)
  "Set the default timezone to one named after string TZ"
  (when (> 2 (hash-table-count local-time::*location-name->timezone*))
    (local-time:reread-timezone-repository))
  (uiop:if-let (tzo (local-time:find-timezone-by-location-name tz))
    (setf local-time:*default-timezone* tzo)))

;; TODO: something to input the timezone and update it (globally?)
#|
(NYC)
(SFO)
(London)
(Paris)
(Athens)
|#
(defun NYC () (set-timezone "US/Eastern")) ; EDT in summer, EST in winter
(defun SFO () (set-timezone "US/Pacific")) ; PDT in summer, PST in winter
(defun London () (set-timezone "Europe/London")) ; BST in summer GMT in winter
(defun Paris () (set-timezone "Europe/Paris")) ; CEST in summer, CET in winter
(defun Athens () (set-timezone "Europe/Athens")) ; EEST in summer, EET in winter

(defmacro def-cli-command (name package &rest wrapper)
  (let ((sym (uiop:find-symbol* name package)))
    `(defcommand ,name () ()
       ,(documentation sym 'function)
       ,(append (or wrapper '(progn))
                `((message (,sym)))))))

(def-cli-command toggle-volume :fare-scripts/audio)
(def-cli-command lower-volume :fare-scripts/audio)
(def-cli-command raise-volume :fare-scripts/audio)
(def-cli-command minimize-volume :fare-scripts/audio)
(def-cli-command maximize-volume :fare-scripts/audio)
(def-cli-command toggle-microphone :fare-scripts/audio)

(def-cli-command brightness-down :fare-scripts/video)
(def-cli-command brightness-up :fare-scripts/video)
(def-cli-command capture-screen :fare-scripts/video)
(def-cli-command lock-screen :fare-scripts/video)

(def-cli-command disable-touchpad :fare-scripts/toggle-touchpad)
(def-cli-command enable-touchpad :fare-scripts/toggle-touchpad)
(def-cli-command stop-chrome :fare-scripts/shell-aliases)
(def-cli-command continue-chrome :fare-scripts/shell-aliases)

(defcommand reconnect-wifi () ()
  "Reconnect wifi"
  ;; We could call (fare-scripts/network:nmup) but that would be synchronous,
  ;; and there are cases where the command times out.
  #|(if (poiu/fork:can-fork-p)
      (let ((pid (posix-fork)))
        (when (= pid -1)
          (fare-scripts/network:nmup)))
      (fare-scripts/network:nmup))|#
  (run-shell-command "PATH=$HOME/bin/nix:$PATH nmup")
  nil)

(defmacro def-activate-command (command &optional arguments class)
  (let* ((command (if (symbolp command) (string-downcase command) command))
         (activator (uiop:intern* (uiop:strcat "ACTIVATE-" (string-upcase command)) :stumpwm-user))
         (class (or class (string-capitalize command))))
    `(defcommand ,activator () ()
       ,(format nil "Run or raise ~A" class)
       (run-or-raise ,(format nil "exec ~A~@[ ~A~]" command arguments) '(:class ,class)))))

(def-activate-command terminator "-l startup")
(def-activate-command emacs)
(def-activate-command chromium-browser)
(def-activate-command brave-browser)
;;(def-activate-command pidgin)
;;(def-activate-command hexchat)

(defmacro with-saved-current-window (() &body body)
  `(call-with-saved-current-window (lambda () ,@body)))

(defvar *last-focused-window* nil) ;; somehow not updated
(defun register-focused-window (new-window &optional current-window)
  (setf *last-focused-window* (or new-window current-window *last-focused-window*)))
(pushnew 'register-focused-window *focus-window-hook*)

(defvar *log* nil)
(defcommand log-current-window-info () ()
  "Log current window info"
  (push (vector (current-window) *last-focused-window*) *log*))
(define-key *root-map* (kbd "y") "log-current-window-info")

(defcommand focus-last-window () ()
  "Focus last windows"
  (focus-window (or (current-window) *last-focused-window*)))
(define-key *root-map* (kbd "Y") "focus-last-window")

(defun call-with-saved-current-window (thunk)
  (register-focused-window (current-window))
  (unwind-protect
       (funcall thunk)
    (sleep 3)
    (focus-last-window)))

(def-cli-command screen-up :fare-scripts/video with-saved-current-window ())
(def-cli-command screen-right :fare-scripts/video with-saved-current-window ())
(def-cli-command screen-down :fare-scripts/video with-saved-current-window ())
(def-cli-command screen-left :fare-scripts/video with-saved-current-window ())
