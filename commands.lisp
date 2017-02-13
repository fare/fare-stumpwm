(in-package :stumpwm)

;; Show current time
(defcommand show-current-time ()
  ()
  (message
   (uiop:with-output (s nil)
     (local-time:format-rfc1123-timestring s (local-time:now))
     ;; Also show battery:
     (format s "~%battery status: ~A~%" (fare-scripts/shell-aliases:battery-status)))))

;;; Sound
(defcommand mute-sound ()
  ()
  (run-shell-command "amixer set Master off")
  (message "toggled sound"))

(defcommand lower-sound ()
  ()
  (run-shell-command "amixer sset Master 5%- on")
  (message "sound 5%-"))

(defcommand raise-sound ()
  ()
  (run-shell-command "amixer sset Master 5%+ on")
  (message "sound 5%+"))

(defcommand minimize-sound ()
  ()
  (run-shell-command "amixer sset Master 100- off")
  (message "sound minimized"))

(defcommand maximize-sound ()
  ()
  (run-shell-command "amixer sset Master 100+ on")
  (message "sound maximized"))

;;; Brightness
(defcommand brightness-down ()
  ()
  (run-shell-command "xbacklight -dec 10")
  (message "brightness down"))

(defcommand brightness-up ()
  ()
  (run-shell-command "xbacklight -inc 10")
  (message "brightness up"))

;;; Screen capture
(defcommand capture-screen ()
  ()
  (run-shell-command "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/DL/screencap/'"))

;;; Screen saver
(defcommand screen-saver ()
  ()
  (run-shell-command "xscreensaver-command -l"))

;;; Applications
(defcommand activate-terminator ()
  ()
  "Run or raise Terminator"
  (run-or-raise "terminator" '(:class ".terminator-wrapped")))

(defcommand activate-emacs ()
  ()
  "Run or raise Emacs"
  (run-or-raise "emacs" '(:class "Emacs")))

(defcommand activate-chromium ()
  ()
  "Run or raise Chromium"
  (run-or-raise "chromium-browser" '(:class "Chrome")))
