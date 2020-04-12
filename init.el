;; Add the current dir to the load-path
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path (expand-file-name "lisp"
                        (expand-file-name "org"
                        (expand-file-name "src" dotfiles-dir))))

;; Load Org-Mode and Org-Babel
(require 'org-install)

;; Processed the emacs config
(org-babel-load-file (expand-file-name "my-stock-changes.org" dotfiles-dir))
(org-babel-load-file (expand-file-name "my-usepackages.org" dotfiles-dir))
(load "~/.emacs.d/elisp-goodies.el")
