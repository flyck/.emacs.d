;; tea-timer (after X minutes, no repeat)
(defun fbr/tea-timer(minutes)
  "my function for timing tee"
  ;; setup
  (require 'alert)
  (setq alert-default-style 'libnotify)
  ;; actual command
  (run-with-timer (* minutes 60) nil 'alert "Tea is ready :)" :title "Tea-time")
  )
