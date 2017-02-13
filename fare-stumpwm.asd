;;; -*- Lisp -*-
#| To use this system, add the following lines to ~/.stumpwmrc

(uiop:call-image-restore-hook)
(asdf:load-system "fare-stumpwm")
(stumpwm:call-stumpwm-start-hook)

|#


(defsystem "fare-stumpwm"
  :license "MIT or bugroff"
  :author "Francois-Rene Rideau"
  :version "0.0.1"
  :depends-on ("stumpwm" "fare-scripts" "local-time")
  :components
  ((:file "general")
   (:file "commands")
   (:file "keys")
   (:file "windows")
   (:file "swank")
   (:file "startup"))
  :description "Fare's configuration for stumpwm"
  :serial t)
