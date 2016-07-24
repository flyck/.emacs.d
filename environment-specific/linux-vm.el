;; To be loaded when I'm on a generic linux-VM (mostly at work)

;; Coding System
;; Set it to utf-8 since otherwise org-codeblock dont work
(prefer-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

;; Org-mode setting in case i do local documentation
;; Its more convenient to have these instead of just TODO and DONE
(setq org-todo-keywords
      '((sequence "TODO" "PENDING" "|" "CANCELED" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("PENDING" . "#f0c674")
        ("CANCELED" . (:foreground "#b5bd68" :weight bold))))
