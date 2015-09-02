;;(require 'fakecygpty)
;;(fakecygpty-activate)

;; http://www.emacswiki.org/emacs/NTEmacsWithCygwin
;;(eval-after-load "tramp"
;;    '(progn
;;       (add-to-list 'tramp-methods
;;                    (mapcar
;;                     (lambda (x)
;;                       (cond
;;                        ((equal x "sshx") "cygssh")
;;                        ((eq (car x) 'tramp-login-program) (list 'tramp-login-program "fakecygpty ssh"))
;;                        (t x)))
;;                     (assoc "sshx" tramp-methods)))
;;      (setq tramp-default-method "cygssh")))
