(setq url-proxy-services '(("no_proxy" . "work\\.com")
                           ("http" . "172.16.8.250:3128")
			   ("https" . "172.16.8.250:3128")))

(setq org-todo-keywords
      '((sequence "TODO" "PENDING" "DELEGATED" "|" "CANCELED" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("PENDING" . "#f0c674") ("DELEGATED" . "#81a2be")
        ("CANCELED" . (:foreground "#b5bd68"
		       ;;:strike-through t
		       :weight bold))))

;; Coding System
(prefer-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

(if (eq system-type 'windows-nt)
    (setq org-agenda-files (list "C:\\Users\\FBrilej\\Desktop\\Projekte\\org\\projects.org"
				 "C:\\Users\\FBrilej\\Desktop\\Projekte\\request-tracker\\ticketsystem.org")))

(if (eq system-type 'gnu/linux)
    (setq org-agenda-files (list "~/Documents/org/projects.org"
				 "~/Documents/request-tracker/ticketsystem.org")))

;;(open-file "C:\\Users\\FBrilej\\Desktop\\Projekte\\org\\projects.org")
;;(load-file "~/Documents/request-tracker/ticketsystem.org")
