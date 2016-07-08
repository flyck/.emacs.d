(setq org-agenda-files (list (string "C:/Users/" (getenv "USERNAME") "/Dropbox/org/gtd/tasks.org" )))

;; Coding System
(prefer-coding-system 'utf-8-dos)
(setq coding-system-for-read 'utf-8-dos)
(setq coding-system-for-write 'utf-8-dos)

(setq org-todo-keywords
      '((sequence "TODO" "PENDING" "|" "CANCELED" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("PENDING" . "#f0c674")
        ("CANCELED" . (:foreground "#b5bd68" :weight bold))))
