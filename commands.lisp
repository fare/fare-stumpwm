(in-package :stumpwm)

;; Show current time
(defcommand show-current-time () ()
  "show current time and other status"
  (message
   (uiop:with-output (s nil)
     ;; Show time
     (local-time:format-rfc1123-timestring s (local-time:now))
     (terpri s)
     ;; Show network connections
     (uiop:if-let (connection (ignore-errors (fare-scripts/network:get-wireless-connections)))
       (format s "~&~{Connected to ~A~%~}" connection))
     ;; Show battery status
     (fare-scripts/shell-aliases:battery-status s))))

(defun set-timezone (tz)
  (when (zerop (hash-table-count local-time::*location-name->timezone*))
    (local-time:reread-timezone-repository))
  (setf local-time:*default-timezone* (local-time:find-timezone-by-location-name tz)))

;; TODO: something to input the timezone and update it (globally?)
#|
(stumpwm::set-timezone "US/Eastern")
(stumpwm::set-timezone "US/Pacific")
(stumpwm::set-timezone "Europe/Paris")
(stumpwm::set-timezone "Europe/London")
|#

;;; Sound
(defun volume-status ()
  (message (uiop:run-program `("pamixer" "--get-volume-human")
                             :input nil :output :string :error-output nil :ignore-error-status t)))

(defcommand toggle-volume () ()
  "toggle volume"
  (message (uiop:run-program `("pamixer" "--toggle-mute")
                             :input nil :output :string :error-output nil :ignore-error-status t))
  (volume-status))

(defcommand lower-volume () ()
  "lower volume"
  (uiop:run-program `("pamixer" "--unmute" "--decrease" "5")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (volume-status))

(defcommand raise-volume () ()
  "raise volume"
  (uiop:run-program `("pamixer" "--unmute" "--increase" "5")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (volume-status))

(defcommand minimize-volume () ()
  "minimize volume"
  (uiop:run-program `("pamixer" "--unmute" "--set-volume" "0")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (volume-status))

(defcommand maximize-volume () ()
  "maximize volume"
  (uiop:run-program `("pamixer" "--unmute" "--set-volume" "100")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (volume-status))

(defun microphone-status () ;; TODO: fix that
  (message (uiop:run-program `("pamixer" "--source" "1" "--get-volume-human")
                             :input nil :output :string :error-output nil :ignore-error-status t)))

(defcommand toggle-microphone () ()
  "toggle microphone"
  (uiop:run-program `("pamixer" "--source" "1" "--toggle-mute")
                    :input nil :output nil :error-output nil :ignore-error-status t)
  (microphone-status))

;;; Brightness

;; TODO: (1) move that to fare-scripts (2) make it work automatically on non-intel video cards.
(defvar *brightness-path* "/sys/class/backlight/intel_backlight/brightness")
(defvar *max-brightness-path* "/sys/class/backlight/intel_backlight/max_brightness")
(defun get-brightness () (uiop:read-file-form *brightness-path*))
(defun get-max-brightness () (uiop:read-file-form *max-brightness-path*))
;;(defun set-brightness (b) (with-output-file (o *brightness-path*) (princ b o))) ;; must be done as root
;; TODO: instead, be using a logarithmic scale? 0, 1... 1060
;; (defun f (n) (round (1- (expt (1+ maxbri) (/ n 20))))) ;; for n from 1 to 20, because (f 0) = (f 1) = 0 ?
;; but then need to decompose current level?
;; (defun g (l) (* 20 (log (1+ l) (1+ maxbri)))) ;; <= bad behavior around 0 :-(

(defun set-brightness (b)
  (uiop:run-program `("sudo" "tee" ,*brightness-path*)
                    :input `(,(princ-to-string b)) :output t :error-output t :ignore-error-status t))
(defun fit-bounds (min max n)
  (cond
    ((< n min) min)
    ((> n max) max)
    (t n)))
(defun adjust-brightness (percent)
  (let* ((brightness (get-brightness))
         (max-brightness (get-max-brightness))
         (new-brightness (fit-bounds 0 max-brightness
                                     (+ brightness (round (* max-brightness 1/100 percent))))))
    (set-brightness new-brightness)
    (round (* 100 new-brightness) max-brightness)))
(defcommand brightness-down () ()
  "decrease brightness"
  ;;(run-shell-command "xbacklight -dec 5") (message "brightness down")
  (message (format nil "brightness down to ~A%" (adjust-brightness -5))))

(defcommand brightness-up () ()
  "increase brightness"
  ;;(run-shell-command "xbacklight -inc 5")(message "brightness up")
  (message (format nil "brightness up to ~A%" (adjust-brightness +5))))

;;; Screen capture
(defcommand capture-screen () ()
  "Capture screen"
  (run-shell-command "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/DL/screencap/'"))

;;; Screen saver
(defcommand screen-saver () ()
  "Run screen saver"
  (run-shell-command "xscreensaver-command -l"))

;;; Applications
(defcommand activate-terminator ()
  ()
  "Run or raise Terminator"
  (run-or-raise "terminator -l startup" '(:class "Terminator")))

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

(defcommand disable-touchpad () ()
  "Disable touchpad"
  (fare-scripts/toggle-touchpad:disable-device))

(defcommand enable-touchpad () ()
  "Enable touchpad"
  (fare-scripts/toggle-touchpad:enable-device))

(defcommand stop-chrome () ()
  "Stop Chrome"
  (fare-scripts/shell-aliases:stop-chrome))

(defcommand continue-chrome () ()
  "Continue Chrome"
  (fare-scripts/shell-aliases:continue-chrome))

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

(defcommand screen-up () ()
  "Screen up"
  (with-saved-current-window ()
    (fare-scripts/xrandr:screen-device-up)))
(defcommand screen-right () ()
  "Screen right"
  (with-saved-current-window ()
    (fare-scripts/xrandr:screen-device-right)))
(defcommand screen-down () ()
  "Screen down"
  (with-saved-current-window ()
    (fare-scripts/xrandr:screen-device-down)))
(defcommand screen-left () ()
  "Screen left"
  (with-saved-current-window ()
    (fare-scripts/xrandr:screen-device-left)))
