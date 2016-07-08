;; tea-timer (after X minutes, no repeat)
(defun fbr/tea-timer(minutes)
  "my function for timing tee"
  (interactive "nHow many minutes does the tea need? ")
  ;; setup
  (require 'alert)
  (setq alert-default-style 'libnotify)
  ;; actual command
  ;; smiley-code: \xF0\x9F\x98\x84
  ;; hourglass-code: \xE2\x8C\x9B
  ;; Listing: http://apps.timwhitlock.info/emoji/tables/unicode
  (run-with-timer (* minutes 60) nil 'alert "Tea is ready \xF0\x9F\x98\x84" :title "Tea-time")
  )
