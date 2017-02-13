(uiop:call-image-restore-hook)
(asdf:load-system "load-quicklisp")
(ql:quickload "stumpwm")
(stumpwm:stumpwm)

#| ;; In ~/.stumpwmrc:
(asdf:load-system "fare-stumpwm")
(stumpwm::call-stumpwm-start-hook)
|#
