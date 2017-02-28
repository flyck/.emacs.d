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

(defun fbr/reload-yasnippet ()
  (interactive)
  (yas-recompile-all)
  (yas-reload-all)
  )

;; (shell-command "Msg asd")
;; (call-process "~\\.emacs.d\\notifu\\notifu.exe" nil t nil "/m" "aads" "/d" "10000" "/i" "~\\emacs.d\\emacs.ico")
;; ;; ich brauche start-process
;; (start-process "my-process" "~\\.emacs.d\\notifu\\notifu.exe" "/m" "aads" "/d" "10000" "/i" "~\\emacs.d\\emacs.ico")
;; (start-process "my-process" "~/.emacs.d/notifu/notifu.exe" "/m" "aads" "/d" "10000")

;; (defun execvp (&rest args)
;;   "Simulate C's execvp() function.
;; Quote each argument seperately, join with spaces and call shell-command-to-string to run in a shell."
;;   (let ((cmd (mapconcat 'shell-quote-argument args " ")))
;;     (shell-command-to-string cmd)))

;; (shell-command-to-string "echo ")
;; (start-process "mvn-exec" "~\.emacs.d\notifu\notifu.exe" "/m" "aads" "/d" "10000" )
;; (setq debug-on-error t)


;; Correct orglzy formatting
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#SEC_Contents
(defun fbr/my-orgzly-reformatting ()
  "Reformat the file to undo changes from orgzly"
  (interactive)
  (setq m (mark-marker))
  (beginning-of-buffer)
  (while (re-search-forward "^\*.*" nil t)
    (next-line)
    (if (looking-at "^$")
        ;; delete the current line if it's empty without sending it to kill-ring
        (let ((beg (progn (forward-line 0)
                          (point))))
          (forward-line 1)
          (delete-region beg (point)))
      nil ;; else part does nothing
      )
    )
  (set-mark-command m)
  )
;; The idea of an external script:
;; http://emacs.stackexchange.com/questions/12148/how-to-pretty-format-code-auto-insert-newlines-indent-etc
;; das multiple-line flag aus perl nutzen, einfach ein perl script über die aktuelle datei laufen lassen
