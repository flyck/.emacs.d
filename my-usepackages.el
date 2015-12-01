(require 'use-package)

(use-package helm
  :ensure t
  :init
  (define-key global-map "\C-xb" 'helm-mini)
  :config
  (setq helm-mini-default-sources '(helm-source-buffers-list
				    helm-source-recentf
				    helm-source-bookmarks
				    helm-source-buffer-not-found))
  (helm-mode 1)
  )

(use-package helm-themes
  :ensure t
  :config
  (helm-themes--load-theme "zonokai-blue")
  )
;; Change the cursor after setting the theme
(setq-default cursor-type 'hbar)

(use-package spaceline-config
  :config
  (spaceline-emacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

;; Put this somewhere useful
(add-to-list 'exec-path "C:/Program Files/Git/bin")
;; The Windows User-Home needs to be in some kind of path such that magit finds the .gitconfig
(use-package magit
  :init
  (define-key global-map (kbd "C-c m") 'magit-status)
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  )

(use-package ace-jump-mode
  :ensure t
  :init
  (define-key global-map (kbd "C-M-q") 'ace-jump-mode)
  )

(use-package org
  :ensure t
  :init
  ;; load org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
	 (lisp . t)
	 (sh . t)
	 ))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\M-n" 'org-metadown)
  (define-key global-map "\M-p" 'org-metaup)
  (global-set-key (kbd "<f9>") 'org-todo)
  (setq org-export-coding-system 'utf-8)
  (setq org-log-done t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (global-set-key [(control next)] 'next-buffer) 
  (global-set-key [(control prior)] 'previous-buffer)
  ;; Visual modifications
  ;; Strike through DONE headlines (from sachachuas config)
  (setq org-fontify-done-headline t)
  (custom-set-faces
   ;; '(org-done ((t (:weight normal
   ;;                 :strike-through t))))
;; '(font-lock-comment-face ((t (:foreground "dark slate blue" :slant italic))))
   '(org-document-title ((t (:foreground "ghost white" :weight bold :height 1.44))))
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t)))))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-ellipsis "⤵")
  (setq org-tags-column -93)
  ;; orgmode archive done tasks
  (defun my-org-archive-done-tasks ()
	(interactive)
	(org-map-entries 'org-archive-subtree "/DONE" 'file)
	(org-map-entries 'org-archive-subtree "/CANCELED" 'file)
	(org-map-entries 'org-archive-subtree "/DELEGATED" 'file))
  :config
  (setq org-export-with-sub-superscripts nil)
  )

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(use-package tramp
  :ensure t
  :init
  (setq tramp-verbose 1)
  ;; sshx is the required for cygwin
  (setq default-tramp-method "sshx")
  ;; When connecting to a remote server it usually does source the profile, but for some
  ;; reason doesn't do that for $PATH by default. You'll have to specifically tell tramp
  ;; to do that from your .emacs. with
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :config
  (define-minor-mode sensitive-mode
	"For sensitive files like password lists.
                It disables backup creation and auto saving.

                With no argument, this command toggles the mode.
                Non-null prefix argument turns on the mode.
                Null prefix argument turns off the mode."
	;; The initial value.
	nil
	;; The indicator for the mode line.
	" Sensitive"
	;; The minor mode bindings.
	nil
	(if (symbol-value sensitive-mode)
		(progn
		  ;; disable backups
		  (set (make-local-variable 'backup-inhibited) t)	
		  ;; disable auto-save
		  (if auto-save-default
			  (auto-save-mode -1)))
										;resort to default value of backup-inhibited
	  (kill-local-variable 'backup-inhibited)
										;resort to default auto save setting
	  (if auto-save-default
		  (auto-save-mode 1))))
  )

(use-package yasnippet
  :ensure t
  :init
  (define-key global-map "\C-cy" 'yas/insert-snippet)
  ;;(setq yas-snippet-dirs (append yas-snippet-dirs
  ;;"~/.emacs.d/elpa/yasnippet-20150912.1330/snippets/"))
  :config
  (yas-global-mode 1)
  )

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  )
