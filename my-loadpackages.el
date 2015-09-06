; loading package
(load "~/.emacs.d/my-packages.el")

(require 'color-theme-sanityinc-tomorrow)
;;(load "sanityinc-tomorrow-night-theme")
;;(load "sanityinc-tomorrow-day-theme")
;;(load "sanityinc-tomorrow-night-theme")
;;(load "sanityinc-tomorrow-blue-theme")
(load "sanityinc-tomorrow-bright-theme")
;;(load "sanityinc-tomorrow-eighties-theme")

(require 'helm-themes)
;;(helm-themes--load-theme "sanityinc-tomorrow-eighties")
;;(helm-themes--load-theme "sanityinc-tomorrow-blue")
;;(helm-themes--load-theme "sanityinc-tomorrow-night")
(helm-themes--load-theme "sanityinc-tomorrow-bright")
;;(helm-themes--load-theme "sanityinc-tomorrow-day")

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
;;(setq sml/theme 'dark)
(setq sml/theme 'light)

(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)
;;C:\Users\Omega needs to be in some kind of path such that magit finds the .gitconfig in windows
(setenv "GIT_ASKPASS" "git-gui--askpass")

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "<f9>") 'org-todo)
(setq org-export-coding-system 'utf-8)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; remove indentation of text after headlines
;; (setf org-adapt-indentation nil)
(global-set-key [(control next)] 'next-buffer) 
(global-set-key [(control prior)] 'previous-buffer)
(setq org-export-with-sub-superscripts nil)
;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (lisp . t)
   (sh . t)
   ))
;; orgmode archive done tasks
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
  (org-map-entries 'org-archive-subtree "/CANCELED" 'file)
  (org-map-entries 'org-archive-subtree "/DELEGATED" 'file))
(defun save-macro (name)
  "save a macro. Take a name as argument
   and save the last defined macro under
   this name at the end of your .emacs"
   (interactive "SName of the macro :")  ; ask for the name of the macro
   (kmacro-name-last-macro name)         ; use this name for the macro
   (find-file user-init-file)            ; open ~/.emacs or other user init file
   (goto-char (point-max))               ; go to the end of the .emacs
   (newline)                             ; insert a newline
   (insert-kbd-macro name)               ; copy the macro
   (newline)                             ; insert a newline
   (switch-to-buffer nil))               ; return to the initial buffer
;; Strike through DONE headlines (from sachachuas config)
(setq org-fontify-done-headline t)
(custom-set-faces
;; '(org-done ((t (:weight normal
;;                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
	      (:strike-through t)))))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(require 'tramp)
(setq tramp-verbose 1)
;; sshx is the required for cygwin
(setq default-tramp-method "sshx")
;;When connecting to a remote server it usually does source the profile, but for some reason doesn't do that
;;for $PATH by default. You'll have to specifically tell tramp to do that from your .emacs. with
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

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

(require 'helm-config)
(helm-mode 1)
(setq helm-mini-default-sources '(helm-source-buffers-list
				  helm-source-recentf
				  helm-source-bookmarks
				  helm-source-buffer-not-found))
(define-key global-map "\C-xb" 'helm-mini)

(require 'projectile)
(projectile-global-mode)

;; auto-fill-mode
(setq-default fill-column 99)
(auto-fill-mode 1)
(define-key global-map "\C-cf" 'auto-fill-mode)

;; lets me switch screen configurations
(when (fboundp 'winner-mode)
  (winner-mode 1))

(require 'yasnippet)
;; muss eventuell vor "require yasnippt"
(add-to-list 'load-path
	     "~/.emacs.d/elpa/yasnippet-20150811.1222")
(setq yas-snippet-dirs '("~/.emacs.d/elpa/yasnippet-20150811.1222/snippets/"))
(yas-global-mode 1)
(define-key global-map "\C-cy" 'yas/insert-snippet)

(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; eshell
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
