;; Add the path to the current version of org-mode
;; This is necessary if one doesn't want to use the prepackaged version of org-mode that comes with
;; emacs
(add-to-list 'load-path "~/.emacs.d/elpa/org-20170210/")

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path (expand-file-name "lisp"
                        (expand-file-name "org"
                        (expand-file-name "src" dotfiles-dir))))

;; Load Org-Mode and Org-Babel
(require 'org-install)

;; Now the emacs config can be processed using the Org-Babel-style
(org-babel-load-file (expand-file-name "init.org" dotfiles-dir))
(org-babel-load-file (expand-file-name "my-org-usepackages.org" dotfiles-dir))
(load "~/.emacs.d/elisp-goodies.el")
