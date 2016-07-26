;; To be loaded when I'm using my laptop

;; Coding System
;; (prefer-coding-system 'utf-8-unix)
;; (setq coding-system-for-read 'utf-8-unix)
;; (setq coding-system-for-write 'utf-8-unix)

;; Org-agenda files
(setq org-agenda-files (list "~/Dropbox/org/gtd/tasks.org"
                             "~/Dropbox/org/hobby/dactyl-keyboard-guide/index.org"
			     "~/Dropbox/org/uni/bachelor_thesis/bachelor_thesis.org"))
;; open this file on startup (doesnt work since (package-initialize) has to be executed before)
;;(switch-to-buffer (find-file-noselect "~/Dropbox/org/gtd/tasks.org"))

;; Startup position of emacs
(if (window-system)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) 200))

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "PENDING(p)" "|" "CANCELED(c)" )
        ))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("PENDING" . "#f0c674")
        ("CANCELED" . (:foreground "#b5bd68" :weight bold))))

;; org-capture setup
(setq org-capture-templates
      '(("a" "Add a task to tasks.org." entry
         (file "tasks.org")
         "* TODO %?
SCHEDULED: %t")))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))
