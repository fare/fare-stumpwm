(in-package :stumpwm)

(defparameter *window-class-renumber*
  '(("Terminator" . 0)
    ("Emacs" . 1)
    ("Chromium" . 2))
  "alist of window classes to be renumbered, and their target numbers.")

(defun renumber-window-by-class (win)
  "Renumber window if its class matches *window-class-renumber*."
  (let* ((class (window-class win))
         (target-number (cdr (assoc class *window-class-renumber*
                                    :test #'string=))))
    (when target-number
      (let ((other-win (find-if #'(lambda (win)
                                    (= (window-number win) target-number))
                                (group-windows (window-group win)))))
        (if other-win
            (when (string-not-equal class (window-class other-win))
              ;; other window, different class; switch numbers
              (setf (window-number other-win) (window-number win))
              (setf (window-number win) target-number))
            ;; If there's already a window of this class, do nothing.
            ;; just keep the new number for this window.

            ;; Else: no other window; target number is free.
            (setf (window-number win) target-number))

        ;; Finally
        (update-all-mode-lines)))))

(add-hook *new-window-hook* 'renumber-window-by-class)
