(in-package :stumpwm)

;; Show current time
(defcommand show-current-time ()
  ()
  (message
   (uiop:with-output (s nil)
     ;; Show time
     (local-time:format-rfc1123-timestring s (local-time:now))
     ;; Show network connections
     (format s "~&~{Connected to ~A~%~}" (fare-scripts/network:get-wireless-connections))
     ;; Show battery status
     (fare-scripts/shell-aliases:battery-status s))))

;;; Sound
(defcommand toggle-volume ()
  ()
  (run-shell-command "amixer set Master toggle")
  (message "toggled sound"))

(defcommand lower-volume ()
  ()
  (run-shell-command "amixer sset Master 5%- on")
  (message "sound 5%-"))

(defcommand raise-volume ()
  ()
  (run-shell-command "amixer sset Master 5%+ on")
  (message "sound 5%+"))

(defcommand minimize-volume ()
  ()
  (run-shell-command "amixer sset Master 100- off")
  (message "sound minimized"))

(defcommand maximize-volume ()
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
  (run-or-raise "terminator" '(:class "Terminator")))

(defcommand activate-emacs ()
  ()
  "Run or raise Emacs"
  (run-or-raise "emacs" '(:class "Emacs")))

(defcommand activate-chromium ()
  ()
  "Run or raise Chromium"
  (run-or-raise "chromium-browser" '(:class "Chromium-browser")))

(defcommand activate-pidgin ()
  ()
  "Run or raise Pidgin"
  (run-or-raise "pidgin" '(:class "Pidgin")))

(defcommand activate-hexchat ()
  ()
  "Run or raise Hexchat"
  (run-or-raise "hexchat" '(:class "Hexchat")))

(defcommand lock-screen ()
  ()
  "Lock the screen"
  (run-shell-command "xscreensaver-command -lock"))

(defcommand reconnect-wifi ()
  ()
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
