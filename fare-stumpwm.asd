;;; -*- Lisp -*-
#| To use this system, add the following lines to ~/.stumpwmrc

(uiop:call-image-restore-hook)
(asdf:load-system "fare-stumpwm")
(stumpwm:call-stumpwm-start-hook)

|#

(defsystem "fare-stumpwm"
  :license "MIT" ;; or bugroff
  :author "Francois-Rene Rideau"
  :version "0.1.1"
  :depends-on (;;"poiu" ;; loading it first to speed the build.
               "stumpwm"
               "fare-scripts" "local-time" "ttf-fonts" "swank")
  :components
  ((:file "general")
   (:file "commands")
   (:file "keys")
   (:file "windows")
   (:file "swank")
   (:file "startup"))
  :description "Fare's configuration for stumpwm"
  :serial t)
