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
  (set-face-attribute 'helm-selection nil
                    :background "ivory1"
                    :foreground "gray5")
  (global-set-key (kbd "M-x") 'helm-M-x)
  )

;;(use-package helm-themes
;;  :ensure t
;;  :config
;;  (helm-themes--load-theme "zonokai-blue")
;;  )

;; The Windows User-Home needs to be in some kind of path such that magit finds the .gitconfig
(use-package magit
  :if (cond ((equal "home" (getenv "SYSENV")) (message "Loading magit"))
      ((equal "laptop" (getenv "SYSENV")) (message "Loading magit"))
      ((equal "work" (getenv "SYSENV")) (message "Loading magit"))
      )
  :ensure t
  :config
  (add-to-list 'exec-path "C:/Program Files/Git/bin")
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
  (setq org-export-coding-system 'utf-8-unix)
  ;; Org Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
	 (lisp . t)
	 (sh . t)
	 (perl . t)
	 (dot . t) ;; activates graphviz dot support
	 ))
  ;; Custom commands
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\M-n" 'org-metadown)
  (define-key global-map "\M-p" 'org-metaup)
  (define-key org-mode-map "\C-m" 'nil)
  (define-key org-mode-map (kbd "<f5>") 'org-babel-execute-src-block)
  (global-set-key (kbd "<f9>") 'org-todo)
  ;; Load syntax-highlighting for source-blocks
  (setq org-src-fontify-natively t)
  (setq org-log-done t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  ;; Visual modifications
  ;; Strike through DONE headlines
  (setq org-fontify-done-headline t)
  ;; autofill hooks for automatic indentation
  (add-hook 'change-log-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq auto-hscroll-mode nil)
  (setq org-hide-emphasis-markers t)
  (setq org-tags-column -93)
  ;; change from ... to the arrow
  (setq org-ellipsis "⤵")
  ;; Circulate Bullets instead of asteriks
  (font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Custom fuctions Archive done tasks properly (doesn't work, i use C-x-a and C-x-z z z z z to
  ;; archive multiple entries)
  (defun my-org-archive-done-tasks ()
	(interactive)
	(org-map-entries 'org-archive-subtree "/DONE" 'file)
	(org-map-entries 'org-archive-subtree "/CANCELED" 'file)
	(org-map-entries 'org-archive-subtree "/DELEGATED" 'file)
	)
  ;; what does this even do?
  (setq org-export-with-sub-superscripts nil)
  ;; remove the "validate"-link from the org-html export
  (setq org-export-html-validation-link nil)
  (setq org-tags-match-list-sublevels 'indented)
  ;; A package to visualize repeated tasks in the org agenda
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  )

(use-package smooth-scrolling
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (smooth-scrolling-mode 1)))
  )


;; Settings for company plus company-emoji
(use-package company-emoji
  :if (equal "laptop" (getenv "SYSENV"))
  :init
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
  )

;; I seem to use this on and off from time to time
;; (use-package org-bullets
;;   :init
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;   )

;; Inserts highlighting of Org Source-Blocks on Html-Export
(use-package htmlize
  :ensure t
  )

(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right)
  (setq which-key-popup-type 'side-window)
  (which-key-mode)
  )

(use-package tramp
  :ensure t
  :init
  (setq tramp-verbose 5)
  ;; sshx is the required for cygwin
  (setq default-tramp-method "sshx")
  ;; Fix for base64 error
  ;; see footnotes here: http://www.howardism.org/Technical/Emacs/literate-devops.html
  ;; you can try this:
  ;;(setq tramp-remote-coding-commands '(b64 "base64" "base64 -d -i"))
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
	;; resort to default value of backup-inhibited
	  (kill-local-variable 'backup-inhibited)
	;; resort to default auto save setting
	  (if auto-save-default
		  (auto-save-mode 1))))
  )

;; Currently there is a problem "package does not untar cleanly"
(use-package yasnippet
  :ensure nil
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

;; smartparens, a mode that tries to be smart around parentheses of all kinds
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  ;; make the overlay disappear
  (custom-set-faces '(sp-pair-overlay-face ((t nil))))
  )

;; Theme-config (and old theme configs)

;; required for zonokai
;; (use-package dash
;;   :ensure t
;;   )
;;(use-package zonokai-theme
;;  :ensure t
;;  )

;; (use-package abyss-theme
;;   :ensure t
;;   )

(use-package seti-theme
  :ensure t
  :config
  (custom-set-faces
   '(font-lock-function-name-face ((t (:foreground "royal blue"))))
   '(font-lock-comment-face ((t (:foreground "#9FCA56"))))
   '(helm-source-header ((t (:background "gray14" :foreground "white" :weight bold :height 1.3 :family "Sans Serif"))))
   '(helm-candidate-number ((t (:foreground "goldenrod2"))))
   '(org-level-1 ((t (:height 1.4 :foreground "royal blue"))))
   '(org-level-2 ((t (:inherit outline-2 :foreground "indian red" :height 1.3))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
   '(org-document-title ((t (:foreground "ghost white" :weight bold :height 1.44))))
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t))))
   '(org-date ((t (:foreground "cornflower blue" :underline t))))
   '(org-link ((t (:inherit nil :foreground "cornflower blue"))))
   ;; Color the Org-Blocks beautifully for color schemes that do not do that
   '(org-block-foreground ((t (:foreground "dark orange"))))
   '(org-block-begin-line ((t (:foreground "SlateBlue4"))))
   '(org-block-end-line ((t (:foreground "SlateBlue4"))))
   '(org-document-info ((t (:foreground "medium sea green"))))
   '(org-document-info-keyword ((t (:foreground "#9FCA56"))))
   )
  )

;; The mode-line
;; load it after the theme since themes sometimes set their own mode-line
;; former problem: use-package doesnt find spaceline-config or spaceline
;; doest it still exist?
(use-package 'spaceline-config
  :load
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  )

;; Change the cursor after setting the theme
(setq-default cursor-type 'hbar)
