(uiop:call-image-restore-hook)
(asdf:load-system "stumpwm")
(stumpwm:stumpwm)

#| ;; In ~/.stumpwmrc:
(asdf:load-system "fare-stumpwm")
(stumpwm::call-stumpwm-start-hook)
|#
