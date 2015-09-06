;; Load-Path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
;;(setq exec-path (append exec-path '("C:/Program Files (x86)/Git/bin")))
;;(setq load-path (append load-path '("C:/cygwin/fakecygpty")))
(load "~/.emacs.d/my-loadpackages.el")
;; working on this code:
;; (let (environment ("home"))
;;   (cond
;;    ((eq 'environment "home")
;;    (print "im in a home environment")    
;;    (if (file-exists-p "~/.emacs.d/home.el") 
;; 	(load "~/.emacs.d/home.el")))
;;    ((eq 'environment "work")
;;    (print "im in a work environment")
;;    (if (file-exists-p "~/.emacs.d/work.el")
;;        (load "~/.emacs.d/work.el")))))

;; temporary code to fix ~the problem~
(if (file-exists-p "~/.emacs.d/home.el") 
    (load "~/.emacs.d/home.el"))

;; Coding System
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; stuff
(setq org-log-done t)
(setq inhibit-default-init t)
(setq inhibit-splash-screen t)
(setq transient-mark-mode 1)

;; Dired
(setq dired-listing-switches "-alh")

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))
;; default to unified diffs
(setq diff-switches "-u")

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))
;;(set-frame-font "Source Code Pro-16" nil t)

;; customize the interface on windows
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1))

;; Remove alarm (bell) on scroll
(setq ring-bell-function 'ignore)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-tags-column -93)
 '(package-selected-packages
   (quote
    (org-beautify-theme yasnippet smart-mode-line rainbow-delimiters org-toodledo nyan-mode magit helm-themes helm-projectile color-theme-sanityinc-tomorrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t)))))

;; Fonts
(defun mouse-set-font (&rest fonts)
 ;; "Select an Emacs font from a list of known good fonts and fontsets.
 
;;If `w32-use-w32-font-dialog' is non-nil (the default), use the Windows
;;font dialog to display the list of possible fonts.  Otherwise use a
;;pop-up menu (like Emacs does on other platforms) initialized with
;;the fonts in `w32-fixed-font-alist'.
;;If `w32-list-proportional-fonts' is non-nil, add proportional fonts
;;to the list in the font selection dialog (the fonts listed by the
;;pop-up menu are unaffected by `w32-list-proportional-fonts')."

  (interactive
   (if w32-use-w32-font-dialog
       (let ((chosen-font (w32-select-font (selected-frame)
					   w32-list-proportional-fonts)))
	 (and chosen-font (list chosen-font)))
     (x-popup-menu
      last-nonmenu-event
      ;; Append list of fontsets currently defined.
      ;; Conditional on new-fontset so bootstrapping works on non-GUI compiles
      (if (fboundp 'new-fontset)
      (append w32-fixed-font-alist (list (generate-fontset-menu)))))))
  (if fonts
      (let (font)
	(while fonts
	  (condition-case nil
	      (progn
                (setq font (car fonts))
		(set-default-font font)
                (setq fonts nil))
	    (error (setq fonts (cdr fonts)))))
	(if (null font)
	    (error "Font not found")))))

(if (eq system-type 'windows-nt)
    (set-default-font "-outline-Consolas-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1")
    )
