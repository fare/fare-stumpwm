(in-package :stumpwm)

;;(define-key *root-map* (kbd "h") '*help-map*)

(defparameter *top-map-keys*
  '(#|("M-space" . "only")
    ("M-S-space" . "restart-hard")
    ("C-Escape" . "delete")
    ("C-S-Escape" . "kill-window")|#

    ("M-Tab" . "next")
    ("M-S-Tab" . "prev")
    ("M-quoteleft" . "gnext")
    ("M-S-quoteleft" . "gprev")

    ;; These should already be default.
    ;;("XF86AudioMute" . "mute-sound")
    ;;("XF86AudioLowerVolume" . "lower-volume")
    ;;("XF86AudioRaiseVolume" . "raise-volume")
    ;;("S-XF86AudioLowerVolume" . "minimize-sound")
    ;;("S-XF86AudioRaiseVolume" . "maximize-sound")
    ;;("XF86MonBrightnessDown" . "brightness-down")
    ;;("XF86MonBrightnessUp" . "brightness-up")
    )
  "alist of keyboard commands for *top-map*")

(defparameter *root-map-keys*
  '(("." . "loadrc")
    ("c" . "activate-terminator")
    ("C-c" . "activate-terminator")
    ("C-q" . "quit")
    ("E" . "activate-emacs")
    ("C" . "activate-chromium")
    ("=" . "show-current-time")
    ("e" . "capture-screen"))
  "alist of keyboard commands for *root-map*")

(defun map-keys (map keys)
  "Map the given keyboard commands KEYS into the MAP"
  (loop :for (key . val) :in keys :do
    (define-key map (kbd key) val)))

(defun my-keys ()
  (set-prefix-key (kbd "C-t"))
  (map-keys *top-map* *top-map-keys*)
  (map-keys *root-map* *root-map-keys*)
  nil)

(register-stumpwm-start-hook 'my-keys)
