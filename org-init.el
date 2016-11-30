;; Pfad zur aktuellen Org-Mode-Version laden
;; Dies ist erforderlich, wenn man eine neuere als die mit seinem Emacs mitgelieferte
;; Version des Org-Mode verwenden möchte (was sehr zu empfehlen ist)
(add-to-list 'load-path "~/elisp/org/lisp/")

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path (expand-file-name "lisp"
                        (expand-file-name "org"
                        (expand-file-name "src" dotfiles-dir))))

;; Org-Mode und damit Org-Babel laden
(require 'org-install)

;; Nun kann die Emacs-Konfiguration im Org-Babel-style eingelesen werden
(org-babel-load-file (expand-file-name "init.org" dotfiles-dir))
(org-babel-load-file (expand-file-name "my-org-usepackages.org" dotfiles-dir))
