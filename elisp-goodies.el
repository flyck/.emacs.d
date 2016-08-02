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

(shell-command "Msg asd")
(call-process "~\\.emacs.d\\notifu\\notifu.exe" nil t nil "/m" "aads" "/d" "10000" "/i" "~\\emacs.d\\emacs.ico")
;; ich brauche start-process
(start-process "my-process" "~\\.emacs.d\\notifu\\notifu.exe" "/m" "aads" "/d" "10000" "/i" "~\\emacs.d\\emacs.ico")
(start-process "my-process" "~/.emacs.d/notifu/notifu.exe" "/m" "aads" "/d" "10000")

(defun execvp (&rest args)
  "Simulate C's execvp() function.
Quote each argument seperately, join with spaces and call shell-command-to-string to run in a shell."
  (let ((cmd (mapconcat 'shell-quote-argument args " ")))
    (shell-command-to-string cmd)))

(shell-command-to-string "echo ")
(start-process "mvn-exec" "~\.emacs.d\notifu\notifu.exe" "/m" "aads" "/d" "10000" )
(setq debug-on-error t)
