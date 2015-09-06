(setq url-proxy-services '(("no_proxy" . "work\\.com")
                           ("http" . "172.16.8.250:3128")))

(setq org-todo-keywords
      '((sequence "TODO" "CANCELED" "PENDING" "DELEGATED" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("PENDING" . "#f0c674") ("DELEGATED" . "#81a2be")
        ("CANCELED" . (:foreground "#b5bd68" :weight bold))))

(if (eq system-type 'windows-nt)
    (setq org-agenda-files (list "C:\\Users\\FBrilej\\Desktop\\Projekte\\org\\projects.org")))
(if (eq system-type 'gnu/linux)
    (setq org-agenda-files (list "~/Documents/org/projects.org"
				 "~/Documents/request-tracker/ticketsystem.org")))
