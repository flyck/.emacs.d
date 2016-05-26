(setq url-proxy-services '(("no_proxy" . "work\\.com")
                           ("http" . "172.16.8.250:3128")
			   ("https" . "172.16.8.250:3128")))

;; Work-specific org settings
(setq org-todo-keywords
      '((sequence "TODO" "PENDING" "DELEGATED" "|" "CANCELED" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("PENDING" . "#f0c674") ("DELEGATED" . "#81a2be")
        ("CANCELED" . (:foreground "#b5bd68"
		       ;;:strike-through t
				   :weight bold))))
;; Set the agenda-files on my working laptop (windows) and possible linux VMs respectively
(if (eq system-type 'windows-nt)
    (setq org-agenda-files (list "C:\\Users\\FBrilej\\Desktop\\Projekte\\org\\projects.org"
				 "C:\\Users\\FBrilej\\Desktop\\Projekte\\request-tracker\\ticketsystem.org")))
(if (eq system-type 'gnu/linux)
    (setq org-agenda-files (list "~/Documents/org/projects.org"
				 "~/Documents/request-tracker/ticketsystem.org")))
;; org-capture setup
(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file "projects.org")
         "* TODO %?
SCHEDULED: %t")))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))

;; Open my projects file which contains all of my scheduling
(switch-to-buffer (find-file-noselect "C:\\Users\\FBrilej\\Desktop\\Projekte\\org\\projects.org"))

;; Coding System
;; I use the unix coding system everywhere
;; Pros:
;; - Its easier to copy stuff into the terminal without weird line-ending interaction
;; Cons:
;; - Editing my configs or files from the Windows Editor doesn't work
;; - My colleagues are not able to visit my config files
(prefer-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

;; Startup position of emacs
(if (window-system)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) 120))
;; Experimental stuff below:
;; Maximize the screen on Windows-Systems
;; (add-hook 'after-init-hook '(lambda () (w32-send-sys-command #xf030)))
;; (w32-send-sys-command #xf030)
;; (defun w32-maximize-frame ()
;;   "Maximize the current frame"
;;   (interactive)
;;   (w32-send-sys-command 61488)
;; )
;; (w32-maximize-frame)
