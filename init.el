;; good place to get emacs for windows:
;; http://vgoulet.act.ulaval.ca/en/emacs/windows/
;; breaks html export though for some font lock reason
;; this has no image support but can do html export correctly: http://emacs.link/

(require 'package)

;; Create the elpa directory if it doesnt exist since emacs will otherwise complain while loading the packages
(unless (file-exists-p "~/.emacs.d/elpa")
  (make-directory "~/.emacs.d/elpa"))


;; Coding System (system-specific)
;; Right now I use the unix coding system everywhere, might be subject to change though
;; Pros:
;; - Its easier to copy stuff into the terminal without weird line-ending interaction
;; - Makes org-babel codeblocks work in the first place on linux systems
;; - I edit org-files from linux and on windows, which due to org-babel requiring linux line-endings mak
;; - There is no comfortable way to convert line-endings without me noticing it, so I have to decide for one of the two
;; Cons:
;; - Editing my configs or files from the Windows Default Editor doesn't work
;; - My colleagues are not able to visit my config files since they dont care and only use the dos-coding system
(if (or (equal "home" (getenv "SYSENV"))
        (equal "work" (getenv "SYSENV"))
        (equal "laptop" (getenv "SYSENV"))
        (equal "linux-vm" (getenv "SYSENV"))
        )
    (progn (prefer-coding-system 'utf-8-unix)
	   (setq coding-system-for-read 'utf-8-unix)
	   (setq coding-system-for-write 'utf-8-unix))
  )


;; Org-mode settings (system-specific)

;; Org-todo-keywords
(if (equal "work" (getenv "SYSENV"))
    (progn (setq org-todo-keywords
		 '((sequence "TODO(t)" "PENDING(p)" "DELEGATED(e)" "|" "CANCELED(c)" "DONE(d)"))))
  )
(if (or (equal "home" (getenv "SYSENV"))
        (equal "laptop" (getenv "SYSENV"))
        (equal "linux-vm" (getenv "SYSENV")))
    (progn (setq org-todo-keywords
		 '((sequence "TODO(t)" "|" "DONE(d)")
		   (sequence "PENDING(p)" "|" "CANCELED(c)")
		   ))
	   )
  )
;; Keyword-faces, these can be set independant from the system
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("PENDING" . "#f0c674") ("DELEGATED" . "#81a2be")
        ("CANCELED" . (:foreground "#b5bd68" :weight bold))))

;; Org-agenda-files
;; Including my *.org_archive-file on most systems definately slows generating the agenda down,
;; but this way I can archive tasks whenever I want, while always maintaining a consistent look-back
;; at my previous work (without including the archive-file archived tasks disappear from the agenda).
(if (equal "home" (getenv "SYSENV"))
    (progn (setq org-agenda-files (list
                                   (concat "C:/Users/" (getenv "USERNAME") "/Dropbox/org/gtd/tasks.org")
				   (concat "C:/Users/" (getenv "USERNAME") "/Dropbox/org/hobby/dactyl-keyboard-guide/index.org")
				   ))
	   ;; org-capture setup
	   (setq org-capture-templates
		 '(("a" "Add a task to tasks.org." entry
		    (file "tasks.org")
		    "* TODO %? SCHEDULED: %t")))
	   (setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))
           )
  )
(if (equal "laptop" (getenv "SYSENV"))
    (progn (setq org-agenda-files (list
                                   (concat "/home/" (getenv "USERNAME") "/Dropbox/org/gtd/tasks.org")
                                   (concat "/home/" (getenv "USERNAME") "/Dropbox/org/gtd/tasks.org_archive")
                                   (concat "/home/" (getenv "USERNAME") "/Dropbox/org/hobby/dactyl-keyboard-guide/index.org")
                                   (concat "/home/" (getenv "USERNAME") "/Dropbox/org/uni/bachelor_thesis/bachelor_thesis.org")))
	   ;; org-capture setup
	   (setq org-capture-templates
		 '(("a" "Add a task to tasks.org." entry
		    (file "tasks.org")
		    "* TODO %? SCHEDULED: %t")))
	   (setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))
           )
  )
(if (equal "work" (getenv "SYSENV"))
    (progn (setq org-agenda-files
		 (list (concat "C:\\Users\\" (getenv "USERNAME") "\\Desktop\\Projekte\\org\\projects.org")
		       (concat "C:\\Users\\" (getenv "USERNAME") "\\Desktop\\Projekte\\org\\projects.org_archive")
		       (concat "C:\\Users\\" (getenv "USERNAME") "\\Desktop\\Projekte\\request-tracker\\ticketsystem.org")))
	   ;; org-capture setup
	   (setq org-capture-templates
		 '(("a" "My TODO task format." entry
		    (file "projects.org")
		    "* TODO %?
    SCHEDULED: %t")))
	   (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
	   )
  )


;; Manually installed packages / unsorted stuff (system-specific)
;; Some packages dont install for some systems. It is stupid but here is the workaround.
(if (equal "home" (getenv "SYSENV"))
    (progn
      ;; load my manually installed yasnippet package
      (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
      (require 'yasnippet)
      (message "loading yasnippet"))
  )
(if (equal "" (getenv "SYSENV")) ;; assuming we are on a university pc since we cannot set the SYSENV variable there
    (progn
     ;; test tls connection on windows for successfull download of packages
     ;; makes sure this returns t in the echo area
     (gnutls-available-p)
     (setenv "PATH" (concat (getenv "PATH") ";H:\\Win7PoolData\\Desktop\\emacs\\bin"))
     ;; For Git
     (add-to-list 'exec-path "H:/Win7PoolData/Desktop/PortableGit/mingw64/bin")
     ;; For Graphviz
     (setenv "PATH" (concat (getenv "PATH") ";H:\\Win7PoolData\\Desktop\\GraphViz\\bin"))
     (setq exec-path (append exec-path '("H:/Win7PoolData/Desktop/GraphViz/bin"))))
  )


;; proxy settings (system-specific)
(if (equal "work" (getenv "SYSENV"))
    (setq url-proxy-services '(("no_proxy" . "work\\.com")
                           ("http" . "172.16.8.250:3128")
			   ("https" . "172.16.8.250:3128")))
    )

(setq package-enable-at-startup nil)
(setq package-archives nil)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Disable the updating of the package-list on startup to increase startup time
;;(package-refresh-contents)
;; Problems:
;; yasnippet doesnt unpack from melpa
;; usepackage is only available on melpa
;; smartparens crashes when not installed from melpa

(package-initialize)

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load my use-package definitions
(load "~/.emacs.d/my-usepackages.el")

;; Load my elisp-goodies
(load "~/.emacs.d/elisp-goodies.el")

;; Remove ^M Errors in Babel
(add-to-list 'process-coding-system-alist
      '("bash" . (undecided-unix)))
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq show-trailing-whitespace t)

;; Disable tabs
(setq-default indent-tabs-mode nil)

;; Show matching parenthesis without delay
(setq show-paren-delay 0)
(show-paren-mode t)

;; Save minibuffer history
(savehist-mode 1)
;; Delete duplicates in minibuffer history
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq history-length t)

;; Take the short answer, y/n is yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Overwrite selected text
(delete-selection-mode t)

;; Highlight current line (slows down C-n and C-p immensly)
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#cc0033") ;; crimson

;; Eshell
(add-hook 'eshell-mode-hook '(lambda ()
			       ;; Make the eshell behave like a normal shell
                               (local-set-key (kbd "C-p") 'eshell-previous-input)
                               (local-set-key (kbd "M-p") 'previous-line)
			       (local-set-key (kbd "C-n") 'eshell-next-input)
                               (local-set-key (kbd "M-n") 'next-line)
			       (setq pcomplete-cycle-completions nil)
			       ))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(custom-set-faces
 '(eshell-ls-archive ((t (:foreground "gold1" :weight bold))))
 '(eshell-ls-backup ((t (:foreground "LemonChiffon1"))))
 '(eshell-ls-directory ((t (:foreground "brown1" :weight bold))))
 '(eshell-prompt ((t (:foreground "firebrick" :weight bold))))
 )

;; Emacs Startup changes
(setq inhibit-default-init t)
(setq inhibit-splash-screen t)
(setq transient-mark-mode 1)

;; Line intendation
(setq-default fill-column 98)
(setq auto-hscroll-mode t)
(setq hscroll-step 1)
(auto-fill-mode 1)
(define-key global-map "\C-cf" 'auto-fill-mode)
(setq tab-width 4)

;; Start the emacs server such that i can open new files conveniently using the explorer
(require 'server)
(unless (server-running-p)
  (server-start))
;; remove the annoying prompt that occurs when killing such a file
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Dired
(setq dired-listing-switches "-alh")

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))
;; Default to unified diffs
(setq diff-switches "-u")

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;; customize the interface on windows
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode 1)
  )

;; Improve the scrolling to make emacs feel more like an editor
;; cant install this on my work pc
;;(require 'sublimity)
;;(require 'sublimity-scroll)
;;(sublimity-mode 1)
;; TODO: Fix that i cant scroll all the way up using C-v

;; Remove alarm (bell) on scroll
(setq ring-bell-function 'ignore)

;; Intuitive Buffer-changing
(global-set-key [(control next)] 'next-buffer)
(global-set-key [(control prior)] 'previous-buffer)

;; Fonts
;; (set-frame-font "Source Code Pro-11" nil t)
;; How to install on ubuntu:
;; #!/bin/bash
;; mkdir /tmp/adodefont
;; cd /tmp/adodefont
;; wget https://github.com/adobe-fonts/source-code-pro/archive/2.010R-ro/1.030R-it.zip
;; unzip 1.030R-it.zip
;; mkdir -p ~/.fonts
;; cp source-code-pro-2.010R-ro-1.030R-it/OTF/*.otf ~/.fonts/
;; fc-cache -f -v

;; "Select an Emacs font from a list of known good fonts and fontsets.
(defun mouse-set-font (&rest fonts)
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

;; Windows-specific settings
(if (eq system-type 'windows-nt)
    ;; Set the font
    (set-default-font "-outline-Consolas-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1")
    )

;; stop cursor from blinking
(blink-cursor-mode 0)
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode 0))

;; Startup position of emacs
(if (window-system)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-height (selected-frame) 120))
