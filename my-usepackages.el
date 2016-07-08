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
  (define-key global-map "\C-c\C-s" 'helm-grep-do-git-grep)
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

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  )

(use-package org
  :ensure t
  :init
  ;; load org-babel
  (setq org-export-coding-system 'utf-8-unix)
  (setq org-export-with-clocks t)
  (setq org-agenda-start-with-clockreport-mode t)
  ;; Org Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
	 (lisp . t)
	 (sh . t)
	 (perl . t)
	 (dot . t) ;; activates graphviz dot support
	 ))
  ;; Send stderror into the result drawer instead of an extra window
  (setq org-babel-default-header-args:sh
        '((:prologue . "exec 2>&1") (:epilogue . ":"))
        )
  ;; Custom Keybindings
  (fset 'fbr/convert-listitem-into-checklistitem
        "\355\C-f\C-f[]\C-f\C-b \C-b\C-b \C-a\C-n")
  (global-set-key (kbd "C-c b") 'fbr/convert-listitem-into-checklistitem)
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
  ;; for tabs (should be redundant after removing all of my tabs)
  (font-lock-add-keywords 'org-mode
                          '(("^	 +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; what does this even do?
  (setq org-export-with-sub-superscripts nil)
  ;; remove the "validate"-link from the org-html export
  (setq org-export-html-validation-link nil)
  (setq org-tags-match-list-sublevels 'indented)
  ;; A package to visualize repeated tasks in the org agenda
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  ;; Latex settings (somehow doesn't work if i put it in usepackage definition of org)
(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("bjmarticle"
               "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{natbib}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{geometry}
\\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             )
)

(use-package smooth-scrolling
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (smooth-scrolling-mode 1)))
  )

(use-package org-alert
  ;; package on top of alert.el
  :ensure t
  :init
  ;; set this option from alert.el to make alerts visual
  (setq alert-default-style 'libnotify)
  (setq org-alert-enable t)
  (setq org-alert-interval 60)
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

;; Inserts highlighting of Org Source-Blocks on Html-Export
(use-package htmlize
  :ensure t
  )

;; Respawns the scratch buffer when its killed
;; Feels just right
(use-package immortal-scratch
  :ensure t
  :config
  (immortal-scratch-mode t)
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

;; Theme-config
(use-package seti-theme
  :ensure t
  :config
  (custom-set-faces
   '(font-lock-function-name-face ((t (:foreground "royal blue"))))
   '(font-lock-comment-face ((t (:foreground "light sea green")))) ;9FCA56
   '(helm-source-header ((t (:background "gray14" :foreground "white" :weight bold :height 1.3 :family "Sans Serif"))))
   '(helm-candidate-number ((t (:foreground "goldenrod2"))))
   '(helm-selection ((t (:background "light gray" :foreground "gray5"))))
   '(org-level-1 ((t (:height 1.4 :foreground "royal blue"))))
   '(org-level-2 ((t (:inherit outline-2 :foreground "indian red" :height 1.3))))
   '(outline-3 ((t (:foreground "orchid"))))
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
   '(org-document-info-keyword ((t (:foreground "light sea green"))))
   )
  )

;; The mode-line
;; load it after the theme since themes sometimes set their own mode-line
;; former problem: use-package doesnt find spaceline-config or spaceline
;; doest it still exist?
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-nyan-cat-on)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-position-off)
  )

;; Change the cursor after setting the theme
(setq-default cursor-type 'hbar)
