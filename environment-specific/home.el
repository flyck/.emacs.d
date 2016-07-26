(setq org-agenda-files (list
                        (concat "C:/Users/" (getenv "USERNAME") "/Dropbox/org/gtd/tasks.org")
                        (concat "C:/Users/" (getenv "USERNAME") "/Dropbox/org/hobby/dactyl-keyboard-guide/index.org")
                        ))
(switch-to-buffer (find-file-noselect (concat "C:/Users/" (getenv "USERNAME") "/Dropbox/org/gtd/tasks.org")))

;; Coding System
(prefer-coding-system 'utf-8-dos)
(setq coding-system-for-read 'utf-8-dos)
(setq coding-system-for-write 'utf-8-dos)

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
        (sequence "PENDING(p)" "|" "CANCELED(c)")
        ))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("PENDING" . "#f0c674")
        ("CANCELED" . (:foreground "#b5bd68" :weight bold))))

;; load my manually installed yasnippet package
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
